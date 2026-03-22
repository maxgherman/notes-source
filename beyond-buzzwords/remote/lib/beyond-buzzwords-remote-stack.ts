import * as cdk from "aws-cdk-lib";
import * as ec2 from "aws-cdk-lib/aws-ec2";
import * as iam from "aws-cdk-lib/aws-iam";
import type { Construct } from "constructs";

export class BeyondBuzzwordsRemoteStack extends cdk.Stack {
  constructor(scope: Construct, id: string, props?: cdk.StackProps) {
    super(scope, id, props);

    const vpc = new ec2.Vpc(this, "Vpc", {
      maxAzs: 2,
      natGateways: 0,
      subnetConfiguration: [
        {
          name: "public",
          subnetType: ec2.SubnetType.PUBLIC,
          cidrMask: 24,
        },
      ],
    });

    const securityGroup = new ec2.SecurityGroup(this, "WebSecurityGroup", {
      vpc,
      description: "Allow public HTTP access to the remote publisher target",
      allowAllOutbound: true,
    });

    securityGroup.addIngressRule(
      ec2.Peer.anyIpv4(),
      ec2.Port.tcp(80),
      "Public HTTP access",
    );

    const role = new iam.Role(this, "InstanceRole", {
      assumedBy: new iam.ServicePrincipal("ec2.amazonaws.com"),
      description: "Role for the Beyond the Buzzwords remote publisher instance",
    });

    role.addManagedPolicy(
      iam.ManagedPolicy.fromAwsManagedPolicyName(
        "AmazonSSMManagedInstanceCore",
      ),
    );

    const userData = ec2.UserData.forLinux();
    userData.addCommands(
      "set -euxo pipefail",
      "dnf update -y",
      "dnf install -y curl",
      "curl -sfL https://get.k3s.io | INSTALL_K3S_EXEC='--write-kubeconfig-mode 644' sh -s -",
      "export KUBECONFIG=/etc/rancher/k3s/k3s.yaml",
      "until kubectl get nodes >/dev/null 2>&1; do sleep 5; done",
      "kubectl create namespace beyond-buzzwords --dry-run=client -o yaml | kubectl apply -f -",
      "cat > /tmp/index.html <<'EOF'",
      "<!doctype html>",
      "<html lang=\"en\">",
      "  <head>",
      "    <meta charset=\"utf-8\" />",
      "    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" />",
      "    <title>WAITING</title>",
      "    <style>",
      "      body {",
      "        margin: 0;",
      "        min-height: 100vh;",
      "        display: grid;",
      "        place-items: center;",
      "        background: #f8fafc;",
      "        color: #0f172a;",
      "        font-family: Georgia, 'Times New Roman', serif;",
      "      }",
      "      main {",
      "        width: min(720px, calc(100vw - 3rem));",
      "        padding: 3rem;",
      "        background: white;",
      "        border: 1px solid #cbd5e1;",
      "        box-shadow: 0 24px 60px rgba(15, 23, 42, 0.12);",
      "      }",
      "      h1 {",
      "        margin: 0 0 1rem;",
      "        font-size: clamp(2.5rem, 10vw, 4rem);",
      "        letter-spacing: 0.08em;",
      "      }",
      "      p { line-height: 1.5; }",
      "    </style>",
      "  </head>",
      "  <body>",
      "    <main>",
      "      <h1>WAITING</h1>",
      "      <p>The k3s result site is provisioned and waiting for updates.</p>",
      "    </main>",
      "  </body>",
      "</html>",
      "EOF",
      "cat > /tmp/result.json <<'EOF'",
      '{"status":"waiting"}',
      "EOF",
      "kubectl -n beyond-buzzwords create configmap result-site-content --from-file=index.html=/tmp/index.html --from-file=result.json=/tmp/result.json --dry-run=client -o yaml | kubectl apply -f -",
      "cat > /tmp/result-site.yaml <<'EOF'",
      "apiVersion: apps/v1",
      "kind: Deployment",
      "metadata:",
      "  name: result-site",
      "  namespace: beyond-buzzwords",
      "spec:",
      "  replicas: 1",
      "  selector:",
      "    matchLabels:",
      "      app: result-site",
      "  template:",
      "    metadata:",
      "      labels:",
      "        app: result-site",
      "    spec:",
      "      containers:",
      "        - name: web",
      "          image: nginx:alpine",
      "          ports:",
      "            - containerPort: 80",
      "          volumeMounts:",
      "            - name: content",
      "              mountPath: /usr/share/nginx/html/index.html",
      "              subPath: index.html",
      "            - name: content",
      "              mountPath: /usr/share/nginx/html/result.json",
      "              subPath: result.json",
      "      volumes:",
      "        - name: content",
      "          configMap:",
      "            name: result-site-content",
      "---",
      "apiVersion: v1",
      "kind: Service",
      "metadata:",
      "  name: result-site",
      "  namespace: beyond-buzzwords",
      "spec:",
      "  selector:",
      "    app: result-site",
      "  ports:",
      "    - port: 80",
      "      targetPort: 80",
      "---",
      "apiVersion: networking.k8s.io/v1",
      "kind: Ingress",
      "metadata:",
      "  name: result-site",
      "  namespace: beyond-buzzwords",
      "spec:",
      "  ingressClassName: traefik",
      "  rules:",
      "    - http:",
      "        paths:",
      "          - path: /",
      "            pathType: Prefix",
      "            backend:",
      "              service:",
      "                name: result-site",
      "                port:",
      "                  number: 80",
      "EOF",
      "kubectl apply -f /tmp/result-site.yaml",
      "kubectl -n beyond-buzzwords rollout status deployment/result-site --timeout=180s",
    );

    const instance = new ec2.Instance(this, "WebInstance", {
      vpc,
      vpcSubnets: { subnetType: ec2.SubnetType.PUBLIC },
      securityGroup,
      role,
      userData,
      instanceType: ec2.InstanceType.of(
        ec2.InstanceClass.T4G,
        ec2.InstanceSize.SMALL,
      ),
      machineImage: ec2.MachineImage.latestAmazonLinux2023({
        cpuType: ec2.AmazonLinuxCpuType.ARM_64,
      }),
    });

    new cdk.CfnOutput(this, "InstanceId", {
      description: "Use this for the SSM-based remote publisher",
      value: instance.instanceId,
    });

    new cdk.CfnOutput(this, "PublicIp", {
      value: instance.instancePublicIp,
    });

    new cdk.CfnOutput(this, "PublicUrl", {
      value: `http://${instance.instancePublicIp}`,
    });
  }
}
