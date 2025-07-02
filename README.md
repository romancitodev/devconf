# DevConf 🚀

**The definitive configuration language for modern applications**

DevConf is a powerful, human-friendly configuration language designed for developers who want more than basic key-value pairs. With built-in templating, environment variable interpolation, and expressive syntax, DevConf makes managing complex application configurations simple and maintainable.

> [!WARNING]
> This repo is in **Heavy Development** and in an alpha stage, some things doesn't work yet but will do it soon.
> For more info, go to [The current status](#️-implementation-status)

## ✨ Features

- **🎯 Simple & Intuitive**: Clean syntax that's easy to read and write
- **🔄 Environment Interpolation**: Dynamic values with `${VAR}` syntax and defaults
- **📝 Template System**: Reusable configuration templates with parameters
- **🏗️ Dot Notation**: Hierarchical configuration with `app.database.host` syntax
- **📊 Rich Data Types**: Strings, numbers, booleans, arrays, and nested objects
- **💬 Comments**: Full comment support for documentation
- **🔢 Number Formatting**: Underscore separators for readability (`1_000_000`)
- **⚡ Conditional Logic**: `@if` statements for dynamic configuration
- **🎨 Flexible Syntax**: Multiple ways to express the same configuration

## 🚀 Quick Start

```devconf
# Basic configuration
name: "my-app"
version: "1.0.0"
debug: ${NODE_ENV == "debug"}

# Nested configuration with dot notation
app.port: ${PORT:int || 8080}
app.database.host: "localhost"
app.database.pool_size: 10

# Arrays and objects
features: [auth, logging, metrics]
cache: {
  enabled: true,
  ttl: 300,
  type: "redis"
}

# Templates for reusable configurations
@template service(name, port, database=null):
  services.${name}.port: ${port}
  services.${name}.health_check: "/health"
  @if database != null:
    services.${name}.database: ${database}

# Use templates
@use service("api", 8001, "main_db")
@use service("worker", 8002)
```

## 📖 Language Guide

### Basic Types

```devconf
# Strings (quoted or unquoted)
app_name: "My Application"
environment: production

# Numbers with optional underscores
users_count: 1_000_000
response_time: 45.67
percentage: 0.95

# Booleans
debug_enabled: true
maintenance_mode: false

# Arrays
languages: [rust, python, javascript]
ports: [8080, 8081, 8082]

# Objects
database: {
  host: "localhost",
  port: 5432,
  ssl: true
}
```

### Environment Variables

```devconf
# Simple interpolation
api_key: ${API_KEY}

# With defaults
port: ${PORT:int || 3000}
host: ${HOST || "localhost"}

# Conditional expressions
debug: ${NODE_ENV == "development"}
```

### Templates

Templates allow you to create reusable configuration patterns:

```devconf
@template microservice(name, port, database=null):
  services.${name}.port: ${port}
  services.${name}.health_check: "/health"
  services.${name}.metrics: "/metrics"
  @if database != null:
    services.${name}.database: ${database}

# Usage
@use microservice("user-service", 8001, "users_db")
@use microservice("auth-service", 8002)
```

### Dot Notation

Build hierarchical configurations naturally:

```devconf
# These are equivalent:
app.server.host: "localhost"
app.server.port: 8080

app: {
  server: {
    host: "localhost",
    port: 8080
  }
}
```

### Conditional Configuration

```devconf
@if ${NODE_ENV} == "production":
  logging.level: "warn"
  cache.enabled: true

@if ${FEATURE_FLAGS.new_ui}:
  ui.theme: "modern"
```

## 🏗️ Use Cases

- **Microservices**: Define service configurations with templates
- **Multi-environment**: Use environment interpolation for different deployments
- **Complex Applications**: Leverage hierarchical configuration and conditionals
- **DevOps**: Simplify configuration management across teams

## 🛠️ Implementation Status

### Lexer ✅
- [x] Punctuation tokenization
- [x] String literals (quoted and unquoted)
- [x] Identifiers and keywords
- [x] Numeric literals (integers and floats)
- [x] Boolean literals
- [x] Array and object structures
- [x] Indentation handling

### Parser ✅
- [x] Primitive value parsing
- [x] Comment support
- [x] Array and object parsing
- [x] Environment variable interpolation
- [x] Dot notation
- [ ] Conditional expressions (in progress)
- [ ] Template system (planned)

## 🚧 Roadmap

- [ ] Complete dot notation support
- [ ] Conditional logic (`@if` statements)
- [ ] Template system implementation
- [ ] Expression evaluation
- [ ] CLI tool for validation and conversion
- [ ] IDE extensions (VS Code, Vim, etc.)
- [ ] Integration libraries for popular frameworks

## 🤝 Contributing

DevConf is built with Rust and welcomes contributions! Whether you're interested in:

- Adding new language features
- Improving parser performance
- Writing documentation
- Creating IDE extensions
- Building integrations

## 📝 License

This project is licensed under the [MIT License](LICENSE).

## 🌟 Why DevConf?

Tired of juggling YAML indentation, JSON verbosity, and TOML limitations? DevConf combines the best of all worlds:

- **More expressive than TOML** - Templates and conditionals
- **More reliable than YAML** - No indentation nightmares
- **More readable than JSON** - Comments and flexible syntax
- **More powerful than INI** - Rich data types and nesting

Perfect for modern applications that need sophisticated configuration management without the complexity.