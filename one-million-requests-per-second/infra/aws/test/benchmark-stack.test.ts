import assert from "node:assert/strict";
import test from "node:test";
import { App } from "aws-cdk-lib";
import { Match, Template } from "aws-cdk-lib/assertions";
import { BenchmarkStack } from "../lib/benchmark-stack.js";

function synthesizedTemplate(): Template {
  const app = new App();
  return Template.fromStack(
    new BenchmarkStack(app, "TestStack", { stackName: "test-benchmark" }),
  );
}

test("preserves the benchmark topology and parameter contract", () => {
  const template = synthesizedTemplate();

  template.resourceCountIs("AWS::EC2::Instance", 1);
  template.resourceCountIs("AWS::EC2::LaunchTemplate", 1);
  template.resourceCountIs("AWS::AutoScaling::AutoScalingGroup", 1);
  template.resourceCountIs("AWS::RDS::DBInstance", 1);
  template.resourceCountIs("AWS::ECR::Repository", 1);
  template.hasParameter("LoadGeneratorCount", {
    Type: "Number",
    Default: 4,
    MinValue: 1,
    MaxValue: 16,
  });
  template.hasParameter("BenchmarkAvailabilityZone", {
    Type: "AWS::EC2::AvailabilityZone::Name",
  });
  template.hasParameter("DatabaseSecondaryAvailabilityZone", {
    Type: "AWS::EC2::AvailabilityZone::Name",
  });
  template.hasResourceProperties("AWS::AutoScaling::AutoScalingGroup", {
    DesiredCapacity: { Ref: "LoadGeneratorCount" },
    MinSize: { Ref: "LoadGeneratorCount" },
    MaxSize: { Ref: "LoadGeneratorCount" },
  });
});

test("keeps administration private and restricts data-plane ingress", () => {
  const template = synthesizedTemplate();

  template.hasResourceProperties("AWS::EC2::SecurityGroupIngress", {
    IpProtocol: "tcp",
    FromPort: 8080,
    ToPort: 8080,
    SourceSecurityGroupId: { Ref: "LoadGeneratorSecurityGroup" },
  });
  template.hasResourceProperties("AWS::EC2::SecurityGroup", {
    GroupDescription: "PostgreSQL from the benchmark server only",
    SecurityGroupIngress: [
      Match.objectLike({
        IpProtocol: "tcp",
        FromPort: 5432,
        ToPort: 5432,
        SourceSecurityGroupId: { Ref: "ServerSecurityGroup" },
      }),
    ],
  });

  const json = template.toJSON() as {
    Resources: Record<string, { Type: string; Properties?: Record<string, unknown> }>;
  };
  const ingressResources = Object.values(json.Resources).filter(
    (resource) => resource.Type === "AWS::EC2::SecurityGroupIngress",
  );
  assert.equal(ingressResources.length, 1);
  assert.equal(ingressResources[0]?.Properties?.FromPort, 8080);
});

test("keeps RDS private, ephemeral by default, and conditionally protected", () => {
  const template = synthesizedTemplate();

  template.hasResource("AWS::RDS::DBInstance", {
    DeletionPolicy: "Delete",
    UpdateReplacePolicy: "Delete",
    Properties: Match.objectLike({
      PubliclyAccessible: false,
      AvailabilityZone: { Ref: "BenchmarkAvailabilityZone" },
      StorageEncrypted: true,
      BackupRetentionPeriod: 0,
      DeletionProtection: {
        "Fn::If": ["DatabaseProtectionEnabled", true, false],
      },
    }),
  });
});

test("places benchmark nodes in one AZ with explicit public egress", () => {
  const template = synthesizedTemplate();

  template.hasResourceProperties("AWS::EC2::Instance", {
    AvailabilityZone: { Ref: "BenchmarkAvailabilityZone" },
    NetworkInterfaces: [
      Match.objectLike({
        AssociatePublicIpAddress: true,
        DeviceIndex: "0",
      }),
    ],
  });
  template.hasResourceProperties("AWS::EC2::LaunchTemplate", {
    LaunchTemplateData: Match.objectLike({
      NetworkInterfaces: [
        Match.objectLike({
          AssociatePublicIpAddress: true,
          DeviceIndex: 0,
        }),
      ],
    }),
  });
});

test("retains the output contract consumed by the controller", () => {
  const outputs = (synthesizedTemplate().toJSON() as { Outputs: object }).Outputs;
  const expected = [
    "AvailabilityZone",
    "CacheCapacityBytes",
    "DatabaseEngineVersion",
    "DatabaseEndpoint",
    "DatabasePort",
    "DatabaseInstanceClass",
    "DatabaseIdentifier",
    "DatabaseSecretArn",
    "ImageRepositoryUri",
    "LoadGeneratorAutoScalingGroup",
    "LoadGeneratorInstanceType",
    "ServerDnsName",
    "ServerInstanceId",
    "ServerInstanceType",
    "VpcId",
  ];

  assert.deepEqual(Object.keys(outputs).sort(), expected.sort());
});
