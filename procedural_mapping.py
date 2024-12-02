from dataclasses import dataclass
import re
from typing import List, Dict, Tuple, Set

@dataclass
class LegalElement:
    name: str
    present_validity: float
    absent_validity: float
    essentiality: float  
    certainty: float    # Changed from info_gain
    element_type: str   # natural, accidental, circunstancial
    implications: List[str] = None
    strong_dependencies: List[Tuple[str, float]] = None
    moderate_dependencies: List[Tuple[str, float]] = None
    incompatibilities: List[str] = None

@dataclass
class TimingRequirement:
    element: str
    deadline: int
    dependencies: List[str]
    essentiality: float 
    required_before: Set[str]

@dataclass
class ConflictRisk:
    source: str
    target: str
    risk_type: str
    severity: float
    mitigation_stage: int
    description: str

class LegalAnalyzer:
    def __init__(self):
        self.elements: Dict[str, LegalElement] = {}
        self.procedural_stages = {
            1: "presentacion_demanda",
            2: "asignacion_expediente",
            3: "admision_demanda",
            4: "emplazamiento_demandado",
            5: "contestacion_demanda",
            6: "traslado_contestacion",
            7: "formulacion_replica"
        }
        
    def extract_elements(self, filename: str):
        with open(filename, 'r', encoding='utf-8') as f:
            text = f.read()

        # Updated pattern for new format
        element_pattern = (
            r'Elemento: (.*?)\n'
            r'• Validez con elemento presente: (\d+) \(([\d.]+)%\)\n'
            r'• Validez con elemento ausente: (\d+) \(([\d.]+)%\)\n'
            r'• Índice de esencialidad\.- (.*?): ([\d.]+)\n'
            r'• Índice de certeza obligacional\.- .*?: (-?[\d.]+)'
        )
        
        matches = re.finditer(element_pattern, text, re.MULTILINE | re.DOTALL)
        
        for match in matches:
            try:
                name = match.group(1).strip()
                present_validity = float(match.group(3))
                absent_validity = float(match.group(5))
                element_type = match.group(6).strip()
                essentiality = float(match.group(7))
                certainty = float(match.group(8))
                
                self.elements[name] = LegalElement(
                    name=name,
                    present_validity=present_validity,
                    absent_validity=absent_validity,
                    essentiality=essentiality,
                    certainty=certainty,
                    element_type=element_type,
                    implications=[],
                    strong_dependencies=[],
                    moderate_dependencies=[]
                )
                
            except Exception as e:
                print(f"Error processing element {name}: {e}")

        # Extract other relationships
        self.extract_implications(text)
        self.extract_dependencies(text)
        
    def extract_implications(self, text: str):
        implications_section = re.search(r'III\. RELACIONES DE NECESIDAD JURÍDICA\n-+\n(.*?)\n\nIV\.', text, re.DOTALL)
        if implications_section:
            pattern = r'• (.*?) implica que (.*?)(?=\n|$)'
            for match in re.finditer(pattern, implications_section.group(1)):
                source = match.group(1).strip()
                target = match.group(2).strip()
                if source in self.elements:
                    self.elements[source].implications.append(target)

    def extract_dependencies(self, text: str):
        # Strong dependencies
        strong_section = re.search(r'A\. Dependencias Fuertes.*?\n(.*?)(?=\n\nB\.|\Z)', text, re.DOTALL)
        if strong_section:
            pattern = r'• (.*?) → (.*?) \(coeficiente: ([\d.]+)\)'
            for match in re.finditer(pattern, strong_section.group(1)):
                source = match.group(1).strip()
                target = match.group(2).strip()
                coef = float(match.group(3))
                if source in self.elements:
                    self.elements[source].strong_dependencies.append((target, coef))

        # Moderate dependencies
        moderate_section = re.search(r'B\. Dependencias Moderadas.*?\n(.*?)(?=\n\nV\.|\Z)', text, re.DOTALL)
        if moderate_section:
            pattern = r'• (.*?) → (.*?) \(coeficiente: ([\d.]+)\)'
            for match in re.finditer(pattern, moderate_section.group(1)):
                source = match.group(1).strip()
                target = match.group(2).strip()
                coef = float(match.group(3))
                if source in self.elements:
                    self.elements[source].moderate_dependencies.append((target, coef))

    def analyze_critical_elements(self) -> Dict[str, List[LegalElement]]:
        """First Layer: Critical Analysis using essentiality"""
        sorted_elements = sorted(self.elements.values(), key=lambda x: x.essentiality, reverse=True)
        
        return {
            "high_critical": [e for e in sorted_elements if e.essentiality > 0.40],
            "medium_critical": [e for e in sorted_elements if 0.20 < e.essentiality <= 0.40],
            "low_critical": [e for e in sorted_elements if e.essentiality <= 0.20]
        }

    def analyze_conflicts(self) -> Dict[str, List[ConflictRisk]]:
        conflicts = {
            "direct_incompatibilities": [],
            "validity_risks": [],
            "dependency_conflicts": [],
            "strategic_risks": []
        }

        # Identify validity risks
        for name, element in self.elements.items():
            if element.present_validity < element.absent_validity:
                conflicts["validity_risks"].append(
                    ConflictRisk(
                        source=name,
                        target="",
                        risk_type="validity",
                        severity=element.essentiality,
                        mitigation_stage=3,
                        description=f"Elemento con mayor validez en ausencia ({element.absent_validity}%) que en presencia ({element.present_validity}%)"
                    )
                )

        # Strategic risks for essential elements
        for name, element in self.elements.items():
            if element.essentiality > 0.40 and element.element_type == "Elemento natural":
                conflicts["strategic_risks"].append(
                    ConflictRisk(
                        source=name,
                        target="",
                        risk_type="strategic",
                        severity=element.essentiality,
                        mitigation_stage=1,
                        description="Elemento natural con alta esencialidad - requiere atención prioritaria"
                    )
                )

        return conflicts

    def analyze_critical_elements(self) -> Dict[str, List[LegalElement]]:
        """First Layer: Critical Analysis"""
        sorted_elements = sorted(self.elements.values(), key=lambda x: x.essentiality, reverse=True)
        
        return {
            "high_critical": [e for e in sorted_elements if e.essentiality > 0.40],
            "medium_critical": [e for e in sorted_elements if 0.20 < e.essentiality <= 0.40],
            "low_critical": [e for e in sorted_elements if e.essentiality <= 0.20]
        }

    def analyze_dependencies(self) -> Dict[str, list]:
        """Second Layer: Dependencies Analysis"""
        return {
            "necessity_chains": self._get_necessity_chains(),
            "strong_dependency_chains": self._get_dependency_chains("strong"),
            "moderate_dependency_chains": self._get_dependency_chains("moderate")
        }

    def _get_necessity_chains(self) -> List[Dict]:
        chains = []
        for name, element in self.elements.items():
            if element.implications:
                chains.append({
                    "source": name,
                    "implies": element.implications,
                    "essentiality": element.essentiality
                })
        return sorted(chains, key=lambda x: x["essentiality"], reverse=True)

    def _get_dependency_chains(self, strength: str) -> List[Dict]:
        chains = []
        for name, element in self.elements.items():
            deps = element.strong_dependencies if strength == "strong" else element.moderate_dependencies
            if deps:
                chains.append({
                    "source": name,
                    "dependencies": deps,
                    "essentiality": element.essentiality
                })
        return sorted(chains, key=lambda x: x["essentiality"], reverse=True)

    def analyze_timing_requirements(self) -> Dict[int, Dict[str, List]]:
        """Third Layer: Timing Requirements Analysis"""
        timing = {stage: {
            "must_prove": [],
            "should_prove": [],
            "prepare_for": []
        } for stage in self.procedural_stages.keys()}
        
        critical_analysis = self.analyze_critical_elements()
        dependency_analysis = self.analyze_dependencies()
        
        # t=1 Presentación de demanda
        timing[1] = {
            "must_prove": [
                TimingRequirement(
                    element=elem.name,
                    deadline=1,
                    dependencies=[dep[0] for dep in elem.strong_dependencies] if elem.strong_dependencies else [],
                    essentiality=elem.essentiality,
                    required_before=set([impl for impl in elem.implications] if elem.implications else [])
                )
                for elem in critical_analysis["high_critical"]
            ],
            "should_prove": [
                TimingRequirement(
                    element=elem.name,
                    deadline=3,
                    dependencies=[dep[0] for dep in elem.strong_dependencies] if elem.strong_dependencies else [],
                    essentiality=elem.essentiality,
                    required_before=set([impl for impl in elem.implications] if elem.implications else [])
                )
                for elem in critical_analysis["medium_critical"]
            ],
            "prepare_for": self._get_future_requirements(dependency_analysis)
        }
        
        # t=3 Admisión de demanda
        timing[3]["must_prove"] = [
            req for req in timing[1]["must_prove"]
            if not self._is_proven(req.element)
        ]
        timing[3]["should_prove"] = [
            req for req in timing[1]["should_prove"]
            if not self._is_proven(req.element)
        ]
        
        # t=5 Contestación de demanda
        timing[5]["prepare_for"] = [
            TimingRequirement(
                element=elem.name,
                deadline=7,
                dependencies=[dep[0] for dep in elem.strong_dependencies] if elem.strong_dependencies else [],
                essentiality=elem.essentiality,
                required_before=set()
            )
            for elem in self.elements.values()
            if elem.present_validity < 20 and elem.absent_validity > 50
        ]
        
        return timing

    def _get_future_requirements(self, dependency_analysis: Dict) -> List[TimingRequirement]:
        """Identify elements that will need to be proven based on dependencies"""
        future_reqs = []
        for chain in dependency_analysis["necessity_chains"]:
            for implied in chain["implies"]:
                if implied in self.elements:
                    elem = self.elements[implied]
                    future_reqs.append(
                        TimingRequirement(
                            element=elem.name,
                            deadline=3,  # Default to admisión
                            dependencies=[],
                            essentiality=elem.essentiality,
                            required_before=set()
                        )
                    )
        return future_reqs

    def _is_proven(self, element_name: str) -> bool:
        """Placeholder for tracking proven elements"""
        return False  # In a real system, this would track what's been proven

    def print_conflict_analysis(self, conflicts: Dict[str, List[ConflictRisk]]):
        print("\nFOURTH LAYER: CONFLICT PREVENTION")
        print("================================")

        print("\nDirect Incompatibilities:")
        print("-----------------------")
        for risk in conflicts["direct_incompatibilities"]:
            print(f"\n• Conflict between {risk.source} and {risk.target}")
            print(f"  - Severity: {risk.severity:.2f}")
            print(f"  - Must address by t={risk.mitigation_stage}")
            print(f"  - Note: {risk.description}")

        print("\nValidity Risks:")
        print("-------------")
        for risk in conflicts["validity_risks"]:
            print(f"\n• Risk in {risk.source}")
            print(f"  - Severity: {risk.severity:.2f}")
            print(f"  - Must address by t={risk.mitigation_stage}")
            print(f"  - Note: {risk.description}")

        print("\nDependency Conflicts:")
        print("-------------------")
        for risk in conflicts["dependency_conflicts"]:
            print(f"\n• Dependency risk: {risk.source} → {risk.target}")
            print(f"  - Severity: {risk.severity:.2f}")
            print(f"  - Must address by t={risk.mitigation_stage}")
            print(f"  - Note: {risk.description}")

        print("\nStrategic Risks:")
        print("--------------")
        for risk in conflicts["strategic_risks"]:
            print(f"\n• Strategic risk in {risk.source}")
            print(f"  - Severity: {risk.severity:.2f}")
            print(f"  - Must address by t={risk.mitigation_stage}")
            print(f"  - Note: {risk.description}")

    def print_analysis(self):

        timing_analysis = self.analyze_timing_requirements()
        print("\nTIMING REQUIREMENTS")
        print("==============================")
        
        for stage, requirements in timing_analysis.items():
            print(f"\nt={stage}: {self.procedural_stages[stage].upper()}")
            print("-" * 50)
            
            if requirements["must_prove"]:
                print("\nMust Prove:")
                for req in requirements["must_prove"]:
                    print(f"• {req.element}")
                    print(f"  - Deadline: t={req.deadline}")
                    print(f"  - essentiality: {req.essentiality:.2f}")
                    if req.dependencies:
                        print(f"  - Dependencies: {', '.join(req.dependencies)}")
                    if req.required_before:
                        print(f"  - Required for: {', '.join(req.required_before)}")
            
            if requirements["should_prove"]:
                print("\nShould Prove:")
                for req in requirements["should_prove"]:
                    print(f"• {req.element}")
                    print(f"  - Deadline: t={req.deadline}")
                    print(f"  - essentiality: {req.essentiality:.2f}")
            
            if requirements["prepare_for"]:
                print("\nPrepare For:")
                for req in requirements["prepare_for"]:
                    print(f"• {req.element}")
                    print(f"  - Target date: t={req.deadline}")
                    if req.dependencies:
                        print(f"  - Dependencies: {', '.join(req.dependencies)}")

    def print_complete_analysis(self):
        # Print all four layers
        self.print_analysis()  # Previous layers
        conflicts = self.analyze_conflicts()
        self.print_conflict_analysis(conflicts)

class LegalReporter:
    def __init__(self, analyzer):
        self.analyzer = analyzer

    def generate_strategic_report(self) -> str:
        report = []
        critical_analysis = self.analyzer.analyze_critical_elements()
        timing = self.analyzer.analyze_timing_requirements()
        conflicts = self.analyzer.analyze_conflicts()
        
        # Encabezado
        report.extend(self._generate_header())
        
        # 1. Elementos Fundamentales
        report.extend(self._generate_fundamental_elements(critical_analysis))
        
        # 2. Estrategia Procesal
        report.extend(self._generate_procedural_strategy(timing))
        
        # 3. Riesgos y Preparación
        report.extend(self._generate_risk_analysis(conflicts))
        
        # 4. Recomendaciones Probatorias
        report.extend(self._generate_evidence_recommendations())
        
        return "\n".join(report)

    def _generate_header(self) -> list:
        return [
            "ANÁLISIS ESTRATÉGICO DEL CASO",
            "=====================================\n",
        ]

    def _generate_fundamental_elements(self, critical_analysis) -> list:
        sections = [
            "1. ELEMENTOS FUNDAMENTALES DE LA DEMANDA",
            "----------------------------------------\n",

        ]
        
        # Get most critical element
        if critical_analysis["high_critical"]:
            main_element = critical_analysis["high_critical"][0]
            sections.extend([
            f"ELEMENTO PRINCIPAL: {main_element.name}",
            "\nIMPORTANCIA PARA EL CASO:",
            "• Es el elemento fundamental para la procedencia de la acción",
            f"• Es un elemento altamente técnico y específico: solo el {main_element.present_validity}% de las configuraciones jurídicamente válidas lo contienen",
            f"• Si no se acredita adecuadamente, el {main_element.absent_validity}% de las configuraciones apuntan a una relación jurídica distinta",
            f"• Por su índice de criticidad ({main_element.essentiality:.2f}), es el elemento que más impacta en la validez jurídica del caso",
            "\nIMPLICACIONES PRÁCTICAS:",
            "• Requiere una estrategia probatoria especialmente rigurosa",
            "• La carga probatoria debe enfocarse en sus elementos distintivos",
            "• Debemos anticipar argumentos de la contraparte sobre relaciones jurídicas alternativas",
            "\nDe este elemento dependen las siguientes pretensiones:"
            ])
            
            # Add dependencies
            if main_element.implications:
                for impl in main_element.implications:
                    sections.append(f"  - {impl}")

        return sections

    def _generate_procedural_strategy(self, timing) -> list:
        sections = [
            "\n2. ESTRATEGIA PROCESAL POR ETAPAS",
            "--------------------------------\n"
        ]
        
        # Presentación de Demanda
        sections.extend([
            "PRESENTACIÓN DE DEMANDA:",
            "Lo que debemos probar desde el inicio:"
        ])
        
        if timing[1]["must_prove"]:
            for req in timing[1]["must_prove"]:
                sections.append(f"\n• {req.element}")
                if req.dependencies:
                    sections.append("  Para probarlo necesitamos dar cuenta de:")
                    for dep in req.dependencies:
                        sections.append(f"  - Si {dep}")

        # Admisión
        sections.extend([
            "\nPARA LA ADMISIÓN:",
            "Elementos que deben estar sólidamente establecidos:"
        ])
        
        if timing[3]["must_prove"]:
            for req in timing[3]["must_prove"]:
                sections.append(f"• {req.element}")

        return sections

    def _generate_risk_analysis(self, conflicts) -> list:
        sections = [
            "\n3. RIESGOS Y PREPARACIÓN",
            "------------------------\n",
            "PUNTOS DE ATENCIÓN ESPECIAL:"
        ]
        
        # Direct incompatibilities
        if conflicts["direct_incompatibilities"]:
            sections.append("\nIncompatibilidades a evitar:")
            for risk in conflicts["direct_incompatibilities"]:
                sections.append(f"• No podemos argumentar simultáneamente:")
                sections.append(f"  - {risk.source}")
                sections.append(f"  - {risk.target}")
                sections.append("  Porque son legalmente incompatibles")

        # Strategic risks
        if conflicts["strategic_risks"]:
            sections.append("\nPuntos que requieren evidencia sólida:")
            for risk in conflicts["strategic_risks"]:
                sections.append(f"• {risk.source}")
                sections.append(f"  Razón: {risk.description}")

        return sections

    def _generate_evidence_recommendations(self) -> list:
        sections = [
            "\n4. RECOMENDACIONES PROBATORIAS",
            "-----------------------------\n",
            "DOCUMENTOS Y EVIDENCIAS RECOMENDADAS:"
        ]
        
        # Add specific recommendations based on element type
        for name, element in self.analyzer.elements.items():
            if element.essentiality > 0.40:  # Focus on critical elements
                sections.append(f"\nPara {name}:")
                sections.append("Documentos sugeridos:")
                # Add specific recommendations based on element type
                if "subordinado" in name.lower():
                    sections.extend([
                        "• Documentos que demuestren dirección y control",
                        "• Evidencia de horarios y jornada",
                        "• Comunicaciones con superiores",
                        "• Registros de asistencia"
                    ])
                elif "salario" in name.lower():
                    sections.extend([
                        "• Recibos de pago",
                        "• Estados de cuenta",
                        "• Registros de transferencias",
                        "• Documentos de prestaciones"
                    ])

        return sections

    def save_report(self, filename: str):
        report = self.generate_strategic_report()
        with open(filename, 'w', encoding='utf-8') as f:
            f.write(report)

# Integration with main analysis
if __name__ == "__main__":
    analyzer = LegalAnalyzer()
    analyzer.extract_elements("informe_juridico.txt")
    
    # Generate both technical and strategic analyses
    analyzer.print_complete_analysis()  # Technical analysis
    
    print("\n" + "="*50 + "\n")
    
    # Generate lawyer-friendly report
    reporter = LegalReporter(analyzer)
    reporter.save_report("estrategia_legal.txt")
    print("Reporte estratégico generado en 'estrategia_legal.txt'")
