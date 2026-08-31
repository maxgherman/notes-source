import {
  Aws,
  CfnCondition,
  CfnOutput,
  CfnParameter,
  Fn,
  RemovalPolicy,
  Stack,
  type StackProps,
} from "aws-cdk-lib";
import * as autoscaling from "aws-cdk-lib/aws-autoscaling";
import * as ec2 from "aws-cdk-lib/aws-ec2";
import * as ecr from "aws-cdk-lib/aws-ecr";
import * as iam from "aws-cdk-lib/aws-iam";
import * as rds from "aws-cdk-lib/aws-rds";
import * as route53 from "aws-cdk-lib/aws-route53";
import type { Construct } from "constructs";

const SERVER_DNS_NAME = "server.benchmark.internal";

const cloudWatchAgentConfig =
  '{"agent":{"metrics_collection_interval":1},"metrics":{"append_dimensions":{"InstanceId":"${!aws:InstanceId}"},"metrics_collected":{"cpu":{"measurement":["cpu_usage_idle","cpu_usage_iowait","cpu_usage_user","cpu_usage_system"],"metrics_collection_interval":1,"totalcpu":true},"mem":{"measurement":["mem_used_percent"],"metrics_collection_interval":1},"netstat":{"measurement":["tcp_established","tcp_time_wait"],"metrics_collection_interval":1}}}}';

const serverUserData = `#!/bin/bash
set -euxo pipefail
dnf install -y docker jq amazon-cloudwatch-agent
systemctl enable --now docker amazon-ssm-agent
mkdir -p /etc/million-rps /var/lib/million-rps/results
curl --fail --silent --show-error \\
  https://truststore.pki.rds.amazonaws.com/global/global-bundle.pem \\
  --output /etc/million-rps/rds-ca.pem
chmod 0644 /etc/million-rps/rds-ca.pem
cat >/opt/aws/amazon-cloudwatch-agent/etc/amazon-cloudwatch-agent.json <<'JSON'
${cloudWatchAgentConfig}
JSON
/opt/aws/amazon-cloudwatch-agent/bin/amazon-cloudwatch-agent-ctl \\
  -a fetch-config -m ec2 \\
  -c file:/opt/aws/amazon-cloudwatch-agent/etc/amazon-cloudwatch-agent.json -s
`;

const loadGeneratorUserData = `#!/bin/bash
set -euxo pipefail
dnf install -y docker jq amazon-cloudwatch-agent
systemctl enable --now docker amazon-ssm-agent
mkdir -p /etc/million-rps /var/lib/million-rps/results
chown 65532:65532 /var/lib/million-rps/results
cat >/opt/aws/amazon-cloudwatch-agent/etc/amazon-cloudwatch-agent.json <<'JSON'
${cloudWatchAgentConfig}
JSON
/opt/aws/amazon-cloudwatch-agent/bin/amazon-cloudwatch-agent-ctl \\
  -a fetch-config -m ec2 \\
  -c file:/opt/aws/amazon-cloudwatch-agent/etc/amazon-cloudwatch-agent.json -s
`;

const allIpv4Egress: ec2.CfnSecurityGroup.EgressProperty[] = [
  { ipProtocol: "-1", cidrIp: "0.0.0.0/0" },
];

const rootVolume = {
  deviceName: "/dev/xvda",
  ebs: {
    deleteOnTermination: true,
    encrypted: true,
    volumeSize: 30,
    volumeType: "gp3",
  },
};

export class BenchmarkStack extends Stack {
  public constructor(scope: Construct, id: string, props?: StackProps) {
    super(scope, id, props);

    const amiId = new CfnParameter(this, "AmiId", {
      type: "AWS::SSM::Parameter::Value<AWS::EC2::Image::Id>",
      default:
        "/aws/service/ami-amazon-linux-latest/al2023-ami-kernel-default-x86_64",
      description: "Amazon Linux 2023 x86_64 AMI resolved through SSM.",
    });
    const benchmarkAvailabilityZone = new CfnParameter(
      this,
      "BenchmarkAvailabilityZone",
      {
        type: "AWS::EC2::AvailabilityZone::Name",
        description:
          "Availability Zone for the server, load generators, and primary database.",
      },
    );
    const databaseSecondaryAvailabilityZone = new CfnParameter(
      this,
      "DatabaseSecondaryAvailabilityZone",
      {
        type: "AWS::EC2::AvailabilityZone::Name",
        description: "Distinct Availability Zone for the secondary RDS subnet.",
      },
    );
    const serverInstanceType = new CfnParameter(this, "ServerInstanceType", {
      type: "String",
      default: "c6in.8xlarge",
      description:
        "Server instance; c6in.8xlarge provides 32 vCPU and 50 Gbps networking.",
    });
    const loadGeneratorInstanceType = new CfnParameter(
      this,
      "LoadGeneratorInstanceType",
      {
        type: "String",
        default: "c6in.4xlarge",
        description: "Instance type for every load-generator node.",
      },
    );
    const loadGeneratorCount = new CfnParameter(this, "LoadGeneratorCount", {
      type: "Number",
      default: 4,
      minValue: 1,
      maxValue: 16,
    });
    const databaseInstanceClass = new CfnParameter(
      this,
      "DatabaseInstanceClass",
      { type: "String", default: "db.r7i.2xlarge" },
    );
    const databaseEngineVersion = new CfnParameter(
      this,
      "DatabaseEngineVersion",
      { type: "String", default: "17.10" },
    );
    const databaseStorageGiB = new CfnParameter(this, "DatabaseStorageGiB", {
      type: "Number",
      default: 100,
      minValue: 20,
      maxValue: 65_536,
    });
    const cacheCapacityBytes = new CfnParameter(this, "CacheCapacityBytes", {
      type: "Number",
      default: 536_870_912,
      minValue: 1,
    });
    const protectDatabase = new CfnParameter(this, "ProtectDatabase", {
      type: "String",
      default: "false",
      allowedValues: ["true", "false"],
      description:
        "Set true only after accepting that stack deletion will be blocked.",
    });
    const databaseProtectionEnabled = new CfnCondition(
      this,
      "DatabaseProtectionEnabled",
      { expression: Fn.conditionEquals(protectDatabase.valueAsString, "true") },
    );

    const vpc = new ec2.CfnVPC(this, "Vpc", {
      cidrBlock: "10.42.0.0/16",
      enableDnsHostnames: true,
      enableDnsSupport: true,
      tags: [{ key: "Name", value: Fn.sub("${AWS::StackName}-vpc") }],
    });
    const internetGateway = new ec2.CfnInternetGateway(
      this,
      "InternetGateway",
    );
    const internetGatewayAttachment = new ec2.CfnVPCGatewayAttachment(
      this,
      "InternetGatewayAttachment",
      { internetGatewayId: internetGateway.ref, vpcId: vpc.ref },
    );

    const firstAvailabilityZone = benchmarkAvailabilityZone.valueAsString;
    const publicSubnet = new ec2.CfnSubnet(this, "PublicSubnet", {
      availabilityZone: firstAvailabilityZone,
      cidrBlock: "10.42.0.0/20",
      mapPublicIpOnLaunch: true,
      vpcId: vpc.ref,
      tags: [
        { key: "Name", value: Fn.sub("${AWS::StackName}-benchmark") },
      ],
    });
    const databaseSubnetA = new ec2.CfnSubnet(this, "DatabaseSubnetA", {
      availabilityZone: firstAvailabilityZone,
      cidrBlock: "10.42.16.0/24",
      mapPublicIpOnLaunch: false,
      vpcId: vpc.ref,
      tags: [{ key: "Name", value: Fn.sub("${AWS::StackName}-db-a") }],
    });
    const databaseSubnetB = new ec2.CfnSubnet(this, "DatabaseSubnetB", {
      availabilityZone: databaseSecondaryAvailabilityZone.valueAsString,
      cidrBlock: "10.42.17.0/24",
      mapPublicIpOnLaunch: false,
      vpcId: vpc.ref,
      tags: [{ key: "Name", value: Fn.sub("${AWS::StackName}-db-b") }],
    });
    const publicRouteTable = new ec2.CfnRouteTable(this, "PublicRouteTable", {
      vpcId: vpc.ref,
    });
    const publicDefaultRoute = new ec2.CfnRoute(this, "PublicDefaultRoute", {
      destinationCidrBlock: "0.0.0.0/0",
      gatewayId: internetGateway.ref,
      routeTableId: publicRouteTable.ref,
    });
    publicDefaultRoute.addResourceDependency(internetGatewayAttachment);
    new ec2.CfnSubnetRouteTableAssociation(
      this,
      "PublicRouteTableAssociation",
      { routeTableId: publicRouteTable.ref, subnetId: publicSubnet.ref },
    );

    const placementGroup = new ec2.CfnPlacementGroup(this, "PlacementGroup", {
      strategy: "cluster",
      tags: [
        { key: "Name", value: Fn.sub("${AWS::StackName}-benchmark") },
      ],
    });
    const serverSecurityGroup = new ec2.CfnSecurityGroup(
      this,
      "ServerSecurityGroup",
      {
        groupDescription: "TLS service traffic from benchmark generators only",
        vpcId: vpc.ref,
        securityGroupEgress: allIpv4Egress,
        tags: [{ key: "Name", value: Fn.sub("${AWS::StackName}-server") }],
      },
    );
    const loadGeneratorSecurityGroup = new ec2.CfnSecurityGroup(
      this,
      "LoadGeneratorSecurityGroup",
      {
        groupDescription: "Load generators; no inbound access",
        vpcId: vpc.ref,
        securityGroupEgress: allIpv4Egress,
        tags: [
          { key: "Name", value: Fn.sub("${AWS::StackName}-loadgen") },
        ],
      },
    );
    new ec2.CfnSecurityGroupIngress(this, "ServerIngress", {
      groupId: serverSecurityGroup.ref,
      ipProtocol: "tcp",
      fromPort: 8080,
      toPort: 8080,
      sourceSecurityGroupId: loadGeneratorSecurityGroup.ref,
    });
    const databaseSecurityGroup = new ec2.CfnSecurityGroup(
      this,
      "DatabaseSecurityGroup",
      {
        groupDescription: "PostgreSQL from the benchmark server only",
        vpcId: vpc.ref,
        securityGroupIngress: [
          {
            ipProtocol: "tcp",
            fromPort: 5432,
            toPort: 5432,
            sourceSecurityGroupId: serverSecurityGroup.ref,
          },
        ],
        securityGroupEgress: allIpv4Egress,
        tags: [
          { key: "Name", value: Fn.sub("${AWS::StackName}-database") },
        ],
      },
    );

    const databaseSubnetGroup = new rds.CfnDBSubnetGroup(
      this,
      "DatabaseSubnetGroup",
      {
        dbSubnetGroupDescription: Fn.sub(
          "${AWS::StackName} private database subnets",
        ),
        subnetIds: [databaseSubnetA.ref, databaseSubnetB.ref],
      },
    );
    const databaseMonitoringRole = new iam.CfnRole(
      this,
      "DatabaseMonitoringRole",
      {
        assumeRolePolicyDocument: {
          Version: "2012-10-17",
          Statement: [
            {
              Effect: "Allow",
              Principal: { Service: "monitoring.rds.amazonaws.com" },
              Action: "sts:AssumeRole",
            },
          ],
        },
        managedPolicyArns: [
          "arn:aws:iam::aws:policy/service-role/AmazonRDSEnhancedMonitoringRole",
        ],
      },
    );
    const database = new rds.CfnDBInstance(this, "Database", {
      allocatedStorage: databaseStorageGiB.valueAsString,
      availabilityZone: firstAvailabilityZone,
      autoMinorVersionUpgrade: false,
      backupRetentionPeriod: 0,
      copyTagsToSnapshot: true,
      databaseInsightsMode: "standard",
      dbInstanceClass: databaseInstanceClass.valueAsString,
      dbName: "benchmark",
      dbSubnetGroupName: databaseSubnetGroup.ref,
      deleteAutomatedBackups: true,
      deletionProtection: Fn.conditionIf(
        databaseProtectionEnabled.logicalId,
        true,
        false,
      ),
      enablePerformanceInsights: true,
      engine: "postgres",
      engineVersion: databaseEngineVersion.valueAsString,
      masterUsername: "benchmark_admin",
      manageMasterUserPassword: true,
      monitoringInterval: 1,
      monitoringRoleArn: databaseMonitoringRole.attrArn,
      multiAz: false,
      performanceInsightsRetentionPeriod: 7,
      publiclyAccessible: false,
      storageEncrypted: true,
      storageType: "gp3",
      vpcSecurityGroups: [databaseSecurityGroup.ref],
      tags: [
        { key: "BenchmarkStack", value: Aws.STACK_NAME },
        { key: "BenchmarkRole", value: "database" },
      ],
    });
    database.applyRemovalPolicy(RemovalPolicy.DESTROY);

    const imageRepository = new ecr.CfnRepository(this, "ImageRepository", {
      emptyOnDelete: true,
      imageScanningConfiguration: { scanOnPush: true },
      imageTagMutability: "IMMUTABLE",
      lifecyclePolicy: {
        lifecyclePolicyText: JSON.stringify({
          rules: [
            {
              rulePriority: 1,
              description: "retain ten images",
              selection: {
                tagStatus: "any",
                countType: "imageCountMoreThan",
                countNumber: 10,
              },
              action: { type: "expire" },
            },
          ],
        }),
      },
      tags: [{ key: "BenchmarkStack", value: Aws.STACK_NAME }],
    });
    imageRepository.applyRemovalPolicy(RemovalPolicy.DESTROY);

    const ec2AssumeRolePolicy = {
      Version: "2012-10-17",
      Statement: [
        {
          Effect: "Allow",
          Principal: { Service: "ec2.amazonaws.com" },
          Action: "sts:AssumeRole",
        },
      ],
    };
    const managedInstancePolicies = [
      "arn:aws:iam::aws:policy/AmazonSSMManagedInstanceCore",
      "arn:aws:iam::aws:policy/CloudWatchAgentServerPolicy",
    ];
    const ecrRuntimeStatements = [
      { Effect: "Allow", Action: "ecr:GetAuthorizationToken", Resource: "*" },
      {
        Effect: "Allow",
        Action: [
          "ecr:BatchCheckLayerAvailability",
          "ecr:BatchGetImage",
          "ecr:GetDownloadUrlForLayer",
        ],
        Resource: imageRepository.attrArn,
      },
    ];
    const serverRole = new iam.CfnRole(this, "ServerRole", {
      assumeRolePolicyDocument: ec2AssumeRolePolicy,
      managedPolicyArns: managedInstancePolicies,
      policies: [
        {
          policyName: "BenchmarkRuntime",
          policyDocument: {
            Version: "2012-10-17",
            Statement: [
              ...ecrRuntimeStatements,
              {
                Effect: "Allow",
                Action: "secretsmanager:GetSecretValue",
                Resource: database.getAtt("MasterUserSecret.SecretArn"),
              },
              {
                Effect: "Allow",
                Action: ["ssm:GetParameter", "ssm:GetParameters"],
                Resource: Fn.sub(
                  "arn:${AWS::Partition}:ssm:${AWS::Region}:${AWS::AccountId}:parameter/${AWS::StackName}/tls/*",
                ),
              },
            ],
          },
        },
      ],
    });
    const serverInstanceProfile = new iam.CfnInstanceProfile(
      this,
      "ServerInstanceProfile",
      { roles: [serverRole.ref] },
    );
    const loadGeneratorRole = new iam.CfnRole(this, "LoadGeneratorRole", {
      assumeRolePolicyDocument: ec2AssumeRolePolicy,
      managedPolicyArns: managedInstancePolicies,
      policies: [
        {
          policyName: "BenchmarkRuntime",
          policyDocument: {
            Version: "2012-10-17",
            Statement: [
              ...ecrRuntimeStatements,
              {
                Effect: "Allow",
                Action: ["ssm:GetParameter", "ssm:GetParameters"],
                Resource: Fn.sub(
                  "arn:${AWS::Partition}:ssm:${AWS::Region}:${AWS::AccountId}:parameter/${AWS::StackName}/tls/ca.crt",
                ),
              },
            ],
          },
        },
      ],
    });
    const loadGeneratorInstanceProfile = new iam.CfnInstanceProfile(
      this,
      "LoadGeneratorInstanceProfile",
      { roles: [loadGeneratorRole.ref] },
    );

    const serverInstance = new ec2.CfnInstance(this, "ServerInstance", {
      availabilityZone: firstAvailabilityZone,
      blockDeviceMappings: [rootVolume],
      iamInstanceProfile: serverInstanceProfile.ref,
      imageId: amiId.valueAsString,
      instanceType: serverInstanceType.valueAsString,
      metadataOptions: { httpEndpoint: "enabled", httpTokens: "required" },
      monitoring: true,
      networkInterfaces: [
        {
          associatePublicIpAddress: true,
          deleteOnTermination: true,
          deviceIndex: "0",
          groupSet: [serverSecurityGroup.ref],
          subnetId: publicSubnet.ref,
        },
      ],
      placementGroupName: placementGroup.ref,
      tags: [
        { key: "Name", value: Fn.sub("${AWS::StackName}-server") },
        { key: "BenchmarkStack", value: Aws.STACK_NAME },
        { key: "BenchmarkRole", value: "server" },
      ],
      userData: Fn.base64(Fn.sub(serverUserData)),
    });
    serverInstance.addResourceDependency(publicDefaultRoute);

    const benchmarkZone = new route53.CfnHostedZone(this, "BenchmarkZone", {
      name: "benchmark.internal",
      vpcs: [{ vpcId: vpc.ref, vpcRegion: Aws.REGION }],
    });
    new route53.CfnRecordSet(this, "ServerDnsRecord", {
      hostedZoneId: benchmarkZone.ref,
      name: SERVER_DNS_NAME,
      resourceRecords: [serverInstance.attrPrivateIp],
      ttl: "30",
      type: "A",
    });

    const loadGeneratorLaunchTemplate = new ec2.CfnLaunchTemplate(
      this,
      "LoadGeneratorLaunchTemplate",
      {
        launchTemplateData: {
          blockDeviceMappings: [rootVolume],
          iamInstanceProfile: { arn: loadGeneratorInstanceProfile.attrArn },
          imageId: amiId.valueAsString,
          instanceType: loadGeneratorInstanceType.valueAsString,
          metadataOptions: { httpEndpoint: "enabled", httpTokens: "required" },
          monitoring: { enabled: true },
          networkInterfaces: [
            {
              associatePublicIpAddress: true,
              deleteOnTermination: true,
              deviceIndex: 0,
              groups: [loadGeneratorSecurityGroup.ref],
            },
          ],
          placement: { groupName: placementGroup.ref },
          userData: Fn.base64(Fn.sub(loadGeneratorUserData)),
        },
        tagSpecifications: [
          {
            resourceType: "launch-template",
            tags: [
              { key: "Name", value: Fn.sub("${AWS::StackName}-loadgen") },
            ],
          },
        ],
      },
    );
    const loadGeneratorGroup = new autoscaling.CfnAutoScalingGroup(
      this,
      "LoadGeneratorGroup",
      {
        desiredCapacity: loadGeneratorCount.valueAsString,
        launchTemplate: {
          launchTemplateId: loadGeneratorLaunchTemplate.ref,
          version: loadGeneratorLaunchTemplate.attrLatestVersionNumber,
        },
        maxSize: loadGeneratorCount.valueAsString,
        minSize: loadGeneratorCount.valueAsString,
        vpcZoneIdentifier: [publicSubnet.ref],
        tags: [
          {
            key: "Name",
            value: Fn.sub("${AWS::StackName}-loadgen"),
            propagateAtLaunch: true,
          },
          {
            key: "BenchmarkStack",
            value: Aws.STACK_NAME,
            propagateAtLaunch: true,
          },
          {
            key: "BenchmarkRole",
            value: "loadgen",
            propagateAtLaunch: true,
          },
        ],
      },
    );
    loadGeneratorGroup.addResourceDependency(publicDefaultRoute);

    const output = (logicalId: string, value: string): void => {
      const resource = new CfnOutput(this, `Output${logicalId}`, { value });
      resource.overrideLogicalId(logicalId);
    };
    output("AvailabilityZone", firstAvailabilityZone);
    output("CacheCapacityBytes", cacheCapacityBytes.valueAsString);
    output("DatabaseEngineVersion", databaseEngineVersion.valueAsString);
    output("DatabaseEndpoint", database.attrEndpointAddress);
    output("DatabasePort", database.attrEndpointPort);
    output("DatabaseInstanceClass", databaseInstanceClass.valueAsString);
    output("DatabaseIdentifier", database.ref);
    output(
      "DatabaseSecretArn",
      database.getAtt("MasterUserSecret.SecretArn").toString(),
    );
    output("ImageRepositoryUri", imageRepository.attrRepositoryUri);
    output("LoadGeneratorAutoScalingGroup", loadGeneratorGroup.ref);
    output(
      "LoadGeneratorInstanceType",
      loadGeneratorInstanceType.valueAsString,
    );
    output("ServerDnsName", SERVER_DNS_NAME);
    output("ServerInstanceId", serverInstance.ref);
    output("ServerInstanceType", serverInstanceType.valueAsString);
    output("VpcId", vpc.ref);
  }
}
