# Clase RE que representa una expresión regular
from abc import ABC, abstractmethod
from typing import Dict, Set, List, Union,Optional

# Clase Symbol que representa un símbolo ASCII o ε
class Symbol:
    def __init__(self, char: str):
        if char == "ε" or (len(char) == 1 and ord(char) < 128):
            self.char = char
        else:
            raise ValueError("El símbolo debe ser un carácter ASCII o ε")

    def __eq__(self, other):
        return isinstance(other, Symbol) and self.char == other.char

    def __lt__(self, other):
        return isinstance(other, Symbol) and self.char < other.char

    def __str__(self):
        return self.char

# Clase Alphabet que representa un alfabeto
class Alphabet:
    def __init__(self, symbols=None):
        if symbols is None:
            symbols = set()
        self.symbols = set(symbols)

    def add_symbol(self, symbol: Symbol):
        self.symbols.add(symbol)

    def __contains__(self, symbol):
        return symbol in self.symbols

    def __str__(self):
        return "{" + ", ".join(str(s) for s in self.symbols) + "}"

# Clase Word que representa una cadena de símbolos
class Word:
    def __init__(self, symbols=None):
        if symbols is None:
            symbols = []
        self.symbols = symbols

    def __eq__(self, other):
        return isinstance(other, Word) and self.symbols == other.symbols

    def __str__(self):
        if not self.symbols:
            return "ε"
        return "".join(str(symbol) for symbol in self.symbols)

# Clase Language que representa un lenguaje 
class Language:
    def __init__(self, words=None):
        if words is None:
            words = set()
        self.words = set(words)

    def add_word(self, word: Word):
        self.words.add(word)

    def __contains__(self, word):
        return word in self.words

    def __str__(self):
        return "{" + ", ".join(str(w) for w in self.words) + "}"



# Clase abstracta base para todas las expresiones regulares
class RE(ABC):
    @abstractmethod
    def __str__(self):
        pass

# Clase que representa la expresión regular vacía (Empty)
class Empty(RE):
    def __str__(self):
        return "∅"

# Clase que representa el lenguaje {ε} (Epsilon)
class Epsilon(RE):
    def __str__(self):
        return "ε"

# Clase que representa un símbolo ASCII (Sym)
class Sym(RE):
    def __init__(self, char: str):
        self.char = char

    def __str__(self):
        return self.char

# Clase que representa la concatenación de dos expresiones regulares (Conc)
class Conc(RE):
    def __init__(self, e1: RE, e2: RE):
        self.e1 = e1
        self.e2 = e2

    def __str__(self):
        return f"({self.e1} . {self.e2})"

# Clase que representa la unión de dos expresiones regulares 
class Union(RE):
    def __init__(self, e1: RE, e2: RE):
        self.e1 = e1
        self.e2 = e2

    def __str__(self):
        return f"({self.e1} + {self.e2})"

# Clase que representa la estrella de Kleene de una expresión regular 
class Kleene(RE):
    def __init__(self, e: RE):
        self.e = e

    def __str__(self):
        return f"({self.e})*"


#Ejemplos de uso
r1 = Sym('a')                   # Representa el símbolo 'a'
r2 = Conc(Sym('a'), Sym('b'))   # Representa "a . b"
r3 = Union(Sym('a'), Sym('b'))  # Representa "a + b"
r4 = Kleene(Sym('a'))           # Representa "a*"
r5 = Kleene(r3)                 # Representa "(a + b)*"
print("Ejemplos de uso de Expresiones Regulares ")
print(r1)  # Salida: a
print(r2)  # Salida: (a . b)
print(r3)  # Salida: (a + b)
print(r4)  # Salida: (a)*
print(r5)  # Salida: ((a + b))*

# Clase AutomataFinito como base para AFD y AFNE


# Clase para representar un símbolo o una transición epsilon
class CharWithEpsilon:
    def __init__(self, char: Optional[str] = None):
        self.char = char  # Si char es None, representa una transición epsilon

    def __str__(self):
        return self.char if self.char else 'ε'

    def __eq__(self, other):
        return isinstance(other, CharWithEpsilon) and self.char == other.char

    def __hash__(self):
        return hash(self.char)

# Clase base para autómatas finitos
class AutomataFinito:
    def __init__(self, states=None, init_state=None, final_states=None):
        self.states = states if states is not None else []  # Lista de estados
        self.init_state = init_state if init_state is not None else 0  # Estado inicial
        self.final_states = final_states if final_states is not None else set()  # Conjunto de estados finales
        self.delta = {}  # Diccionario para la función de transición

    def set_states(self, states):
        self.states = states

    def set_init_state(self, init_state):
        self.init_state = init_state

    def set_final_states(self, final_states):
        self.final_states = final_states

# Clase DFA que representa un autómata finito determinista
class DFA(AutomataFinito):
    def __init__(self, states=None, init_state=None, final_states=None):
        super().__init__(states, init_state, final_states)
        self.delta = {}  # Dict[str, Dict[Symbol, str]] para la función de transición

    def set_delta(self, delta: Dict[str, Dict[str, str]]):
        self.delta = delta

    def add_transition(self, from_state: str, symbol: str, to_state: str):
        if from_state not in self.delta:
            self.delta[from_state] = {}
        self.delta[from_state][symbol] = to_state

# Clase NFAE que representa un autómata finito no determinista con transiciones epsilon
class NFAE(AutomataFinito):
    def __init__(self, states=None, init_state=None, final_states=None):
        super().__init__(states, init_state, final_states)
        self.delta = {}  # Dict[str, Dict[CharWithEpsilon, Set[str]]] para la función de transición

    def set_delta(self, delta: Dict[str, Dict[CharWithEpsilon, Set[str]]]):
        self.delta = delta

    def add_transition(self, from_state: str, symbol: CharWithEpsilon, to_states: Set[str]):
        if from_state not in self.delta:
            self.delta[from_state] = {}
        if symbol not in self.delta[from_state]:
            self.delta[from_state][symbol] = set()
        self.delta[from_state][symbol].update(to_states)

    def has_epsilon_transitions(self, state: str) -> bool:
        """Verifica si el estado actual puede transicionar con epsilon."""
        return CharWithEpsilon(None) in self.delta.get(state, {})

# Ejemplo de uso
nfae = NFAE(states=['q0', 'q1', 'q2'], init_state='q0', final_states={'q2'})
epsilon = CharWithEpsilon()  # Epsilon
nfae.add_transition('q0', epsilon, {'q1'})
nfae.add_transition('q1', CharWithEpsilon('a'), {'q2'})

print(nfae.has_epsilon_transitions('q0'))  # Salida: True
print(nfae.has_epsilon_transitions('q1'))  # Salida: False



