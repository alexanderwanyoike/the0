# Theo API

Theo API is the backend service for The0 automated trading platform. It is built using the [NestJS](https://nestjs.com/) framework and provides functionality for managing bots, backtesting strategies, and more.

## Features

- **Bot Validation**: Validate bot configurations against schemas retrieved from an external API.
- **Backtesting**: Manage and execute backtesting strategies for trading.
- **Configuration Management**: Centralized configuration using `@nestjs/config`.
- **API Integration**: Communicates with external services using `@nestjs/axios`.

## Prerequisites

- **Node.js**: Version 18 or higher
- **Yarn**: Package manager
- **Firebase CLI**: For deployment and emulation

## Installation

Clone the repository and install dependencies:

```bash
$ git clone <repository-url>
$ cd theo-api
$ yarn install
```

## Environment Variables

The application uses environment variables for configuration. Create a `.env` file in the root directory and define the following variables:

```env
botManagerApi.URL=http://example.com
botManagerApi.APIKey=your-api-key
```

## Running the Application

### Development

Start the application in development mode:

```bash
$ yarn run start:dev
```

### Production

Build and start the application in production mode:

```bash
$ yarn run build
$ yarn run start:prod
```

## Testing

Run unit tests:

```bash
$ yarn run test
```

Run end-to-end (e2e) tests:

```bash
$ yarn run test:e2e
```

Generate test coverage reports:

```bash
$ yarn run test:cov
```

## Project Structure

- **`src/bot`**: Contains bot-related logic, including validation (`bot.validator.ts`).
- **`src/backtest`**: Handles backtesting functionality.
- **`src/common`**: Shared utilities and types.
- **`test`**: Unit and e2e tests.

## Deployment

Deploy the application to Firebase:

```bash
$ yarn run deploy
```

## Contributing

1. Fork the repository.
2. Create a new branch for your feature or bugfix.
3. Submit a pull request.

## License

This project is licensed under the **UNLICENSED** license. See the `LICENSE` file for details.

## Support

For issues or questions, please open an issue in the repository.

## Acknowledgments

- Built with [NestJS](https://nestjs.com/).
- Uses [Ajv](https://ajv.js.org/) for JSON schema validation.


