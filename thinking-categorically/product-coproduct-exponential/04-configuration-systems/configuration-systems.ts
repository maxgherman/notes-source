// Configuration is a product of multiple components
interface DatabaseConfig {
  host: string;
  port: number;
  database: string;
  credentials: { username: string; password: string };
}

interface ServerConfig {
  port: number;
  env: 'development' | 'staging' | 'production';
  logging: { level: string; format: string };
}

interface FeatureFlags {
    flag1: boolean;
    flag2: boolean;
    flag3: boolean;
}

// Application config is a product of sub-configurations
interface AppConfig {
  database: DatabaseConfig;  // Component 1
  server: ServerConfig;      // Component 2
  features: FeatureFlags;    // Component 3
}

// Projection functions extract specific configuration parts
const getDatabaseUrl = (config: AppConfig): string =>
  `${config.database.host}:${config.database.port}/${config.database.database}`;

const getLogLevel = (config: AppConfig): string =>
  config.server.logging.level;
