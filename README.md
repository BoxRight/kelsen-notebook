# Kelsen Notebook

A web-based notebook interface for writing, analyzing, and executing Kelsen code with real-time syntax highlighting and legal analysis features.

## Prerequisites

- Python 3.x
- Flask
- Kelsen compiler (`./kelsen` executable)
- Contract generator (`contract_generator.py`)
- Deontic logic analyzer (`./deonticLogic` executable)
- Procedural mapping script (`procedural_mapping.py`)

## Installation

1. Install Flask:
```bash
pip install flask
```

2. Clone the repository or create the project structure:
```
kelsen-notebook/
├── app.py
├── templates/
│   └── index.html
├── notebooks/
├── kelsen
├── deonticLogic
├── contract_generator.py
└── procedural_mapping.py
```

3. Make sure executables have proper permissions:
```bash
chmod +x ./kelsen
chmod +x ./deonticLogic
```

## Features

- Real-time syntax highlighting for Kelsen code
- Line numbering
- Code validation and analysis
- Contract generation from Kelsen code
- Legal analysis through deontic logic
- Timeline analysis
- Code saving and downloading

## Usage

1. Start the Flask server:
```bash
python app.py
```

2. Open a web browser and navigate to:
```
http://localhost:5000
```

3. Use the notebook interface to:
   - Write Kelsen code with syntax highlighting
   - Execute code to generate contracts and legal analysis
   - Save and download code files

## Code Validation

The notebook validates Kelsen syntax including:
- String declarations
- Asset declarations
- Subject declarations
- Clause declarations
- Semicolon usage
- Brace matching

## Execution Pipeline

When code is executed, it goes through:
1. Kelsen compilation
2. Contract generation
3. Deontic logic analysis
4. Procedural timeline creation
5. Legal report generation

## File Outputs

Execution generates several files:
- `generated_contract.txt`: Natural language contract
- `informe_juridico.txt`: Legal analysis report
- `estrategia_legal.txt`: Legal strategy and timeline

## Error Handling

The notebook provides error feedback for:
- Syntax errors
- Compilation errors
- Missing dependencies
- Execution failures
- File operations

## Project Structure

```
kelsen-notebook/
├── app.py                 # Flask application
├── templates/
│   └── index.html        # Notebook interface
├── notebooks/            # Saved notebooks
├── kelsen               # Kelsen compiler
├── deonticLogic         # Deontic logic analyzer
├── contract_generator.py # Contract generation
└── procedural_mapping.py # Timeline analysis
```

## Contributing

1. Fork the repository
2. Create a feature branch
3. Commit your changes
4. Push to the branch
5. Create a Pull Request

## License

This project is licensed under the MIT License.

## Support

For issues and questions, please open an issue in the repository.
