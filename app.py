from flask import Flask, render_template, request, jsonify, send_file
import re
import subprocess
import tempfile
import os
import logging
import uuid

app = Flask(__name__)

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

# Kelsen code patterns for validation
KELSEN_PATTERNS = {
    'string': r'string\s+\w+\s*=\s*"[^"]*"\s*;',
    'asset': r'asset\s+\w+\s*=\s*\w+\s*,\s*\w+\s*,\s*\w+\s*,\s*\w+\s*,\s*\w+\s*;',
    'subject': r'subject\s+\w+\s*=\s*"[^"]*"\s*,\s*"[^"]*"\s*,\s*\d+\s*,\s*"[^"]*"\s*;',
    'clause': r'clause\s+\w+\s*=\s*\{[^}]*\}\s*;'
}
def validate_kelsen_code(code):
    """Validate Kelsen code and return any errors"""
    errors = []
    lines = code.split('\n')
    
    for i, line in enumerate(lines, 1):
        line = line.strip()
        if not line:
            continue
            
        # Check for missing semicolons
        if line and not line.endswith(';') and not line.endswith('}'):
            errors.append(f"Line {i}: Missing semicolon")
            
        # Check for unmatched braces
        if line.count('{') != line.count('}'):
            errors.append(f"Line {i}: Unmatched braces")
            
        # Validate declarations
        if line.startswith('string') and not re.match(KELSEN_PATTERNS['string'], line):
            errors.append(f"Line {i}: Invalid string declaration")
        elif line.startswith('asset') and not re.match(KELSEN_PATTERNS['asset'], line):
            errors.append(f"Line {i}: Invalid asset declaration")
        elif line.startswith('subject') and not re.match(KELSEN_PATTERNS['subject'], line):
            errors.append(f"Line {i}: Invalid subject declaration")
        elif line.startswith('clause') and not re.match(KELSEN_PATTERNS['clause'], line):
            errors.append(f"Line {i}: Invalid clause declaration")
            
    return errors

def analyze_code(code):
    """Analyze Kelsen code and return metrics"""
    return {
        'strings': len(re.findall(r'string\s+\w+', code)),
        'assets': len(re.findall(r'asset\s+\w+', code)),
        'subjects': len(re.findall(r'subject\s+\w+', code)),
        'clauses': len(re.findall(r'clause\s+\w+', code)),
    }

@app.route('/')
def index():
    return render_template('index.html')

@app.route('/api/analyze', methods=['POST'])
def analyze():
    try:
        code = request.json.get('code', '')
        if not code:
            return jsonify({'error': 'No code provided'})
            
        # Validate and analyze code
        errors = validate_kelsen_code(code)
        metrics = analyze_code(code)
        
        return jsonify({
            'success': True,
            'errors': errors,
            'metrics': metrics
        })
        
    except Exception as e:
        logger.exception("Error analyzing code")
        return jsonify({'error': str(e)})

@app.route('/execute/<code_id>', methods=['POST'])
def execute_kelsen(code_id):
    try:
        data = request.get_json()
        if not data or 'code' not in data:
            return jsonify({
                'success': False,
                'error': 'No code provided'
            }), 400

        generated_code = data['code']
        temp_dir = tempfile.gettempdir()
        kelsen_file = os.path.join(temp_dir, f"kelsen_code_{code_id}.kelsen")
        print(kelsen_file)
        try:
            # Step 1: Write Kelsen code to file
            with open(kelsen_file, 'w', encoding='utf-8') as f:
                f.write(generated_code)

            # Step 2: Execute Kelsen
            kelsen_result = subprocess.run(
                ['./kelsen', kelsen_file],
                capture_output=True,
                text=True,
                check=True
            )
            logger.info("Kelsen execution successful")

            # Step 3: Execute contract generator
            contract_result = subprocess.run(
                ['python', 'contract_generator.py'],
                capture_output=True,
                text=True,
                check=True
            )
            
            # Read generated contract
            with open('generated_contract.txt', 'r', encoding='utf-8') as f:
                contract_text = f.read()

            # Step 4: Execute deontic logic analysis (now non-interactive)
            deontic_result = subprocess.run(
                ['./deonticLogic'],
                capture_output=True,
                text=True,
                check=True
            )

            # Step 5: Execute procedural timeline analysis
            timeline_result = subprocess.run(
                ['python', 'procedural_mapping.py'],
                capture_output=True,
                text=True,
                check=True
            )
            
            # Read legal report
            if os.path.exists('informe_juridico.txt'):
                with open('informe_juridico.txt', 'r', encoding='utf-8') as f:
                    legal_report = f.read()
            else:
                raise FileNotFoundError("Legal report file not generated")
            
            if os.path.exists('estrategia_legal.txt'):
                with open('estrategia_legal.txt','r', encoding='utf-8') as f:
                    legal_strategy = f.read()
            else:
                raise FileNotFoundError("Legal strategy not generated")

            return jsonify({
                'success': True,
                'natural_language': contract_text,
                'legal_analysis': legal_report,
                'kelsen_output': kelsen_result.stdout,
                'timeline_result': legal_strategy
            })

        except subprocess.CalledProcessError as e:
            error_message = f"Process error: {e.stderr if e.stderr else str(e)}"
            logger.error(f"Error in execution pipeline: {error_message}")
            return jsonify({
                'success': False,
                'error': error_message
            })
        except FileNotFoundError as e:
            logger.error(f"File error: {str(e)}")
            return jsonify({
                'success': False,
                'error': f"File error: {str(e)}"
            })
        finally:
            # Cleanup temporary files
            for file in [kelsen_file, 'ast_output.json', 'generated_contract.txt', 'informe_jurídico.txt', 'estrategia_legal.txt']:
                try:
                    if os.path.exists(file):
                        os.remove(file)
                        logger.debug(f"Cleaned up file: {file}")
                except Exception as e:
                    logger.error(f"Error cleaning up {file}: {str(e)}")

    except Exception as e:
        logger.exception("Error in execute endpoint")
        return jsonify({
            'success': False,
            'error': str(e)
        })

@app.route('/api/save', methods=['POST'])
def save():
    try:
        code = request.json.get('code', '')
        if not code:
            return jsonify({'error': 'No code provided'})
            
        filename = f"kelsen_code_{uuid.uuid4()}.kelsen"
        file_path = os.path.join('notebooks', filename)
        
        # Ensure directory exists
        os.makedirs('notebooks', exist_ok=True)
        
        # Save the file
        with open(file_path, 'w') as f:
            f.write(code)
            
        return jsonify({
            'success': True,
            'filename': filename,
            'message': f'Saved as {filename}'
        })
        
    except Exception as e:
        logger.exception("Error saving code")
        return jsonify({'error': str(e)})

if __name__ == '__main__':
    app.run(debug=True)
