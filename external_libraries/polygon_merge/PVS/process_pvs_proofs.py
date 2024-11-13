#!/usr/bin/python3
import re
from utilities import count_parens, find_PVS_log_files
from dataclasses import dataclass
from enum import Enum

@dataclass
class Formula:
    tag: int
    formula: str

@dataclass
class ProofStep:
    antecedents: []
    consequents: []
    command: str

class ExpectedNext(Enum):
    ANTECEDENT = 1
    CONSEQUENT = 2
    COMMAND = 3

TURNSTILE = '  |-------'
CMD_INTRO = 'Rerunning step: '
POSTPONE_CMD = '(postpone)'
CI_LEN = len(CMD_INTRO)
POSTPONE_INTRO = 'Postponing '
FORMULA_PTN = r'^(\[|\{)(-?[0-9]+)(\]|\})(.*)'
DEBUG_LINE = -999

def print_formula(f):
    print(f'{f.tag}: {f.formula}')

def print_proof_step(proof_step):
    for a in proof_step.antecedents:
        print_formula(a)
    print(TURNSTILE)
    for c in proof_step.consequents:
        print_formula(c)
    print(proof_step.command)

def build_proof_steps():
    entries = find_PVS_log_files()
    inside_stmt = False # formula or command
    expecting = ExpectedNext.ANTECEDENT
    new_formula = Formula(0, "")
    results = [] # Array of ProofSteps
    proof_step = ProofStep([], [], "")
    current_txt = ""
    command_parens = 0
    for entry in entries:
        print(f'Processing {entry}...')
        with open(entry, 'r') as f:
            line_number = 0
            for line in f:
                line_number += 1
                if abs(DEBUG_LINE - line_number) <= 3:
                    print(f'{line_number}: line = "{line}", expecting = {expecting}')
                match = re.search(FORMULA_PTN, line)
                if match is not None:
                    if abs(DEBUG_LINE - line_number) <= 3:
                        print(f'{line_number}: Matched for formula, inside_stmt = {inside_stmt}')
                    if inside_stmt:
                        new_formula = Formula(formula_num, current_txt)
                        is_antecedent = formula_num.startswith('-')
                        if is_antecedent:
                            proof_step.antecedents.append(new_formula)
                        else:
                            proof_step.consequents.append(new_formula)
                    formula_num = match.group(2)
                    is_antecedent = formula_num.startswith('-')
                    if is_antecedent:
                        if expecting == ExpectedNext.COMMAND:
                            expecting = ExpectedNext.ANTECEDENT
                        elif expecting != ExpectedNext.ANTECEDENT:
                            print(f'{line_number}: Encountered antecedent, expecting {expecting}')
                    current_txt = match.group(4).strip()
                    inside_stmt = True
                    command_parens = 0
                elif line.startswith(TURNSTILE):
                    if inside_stmt:
                        if expecting == ExpectedNext.ANTECEDENT:
                            new_formula = Formula(formula_num, current_txt)
                            proof_step.antecedents.append(new_formula)
                        else:
                            print(f'{line_number}: Was not expecting turnstile, expecting {expecting}')
                    inside_stmt = False
                    expecting = ExpectedNext.CONSEQUENT
                    command_parens = 0
                elif line.startswith(CMD_INTRO):
                    if inside_stmt:
                        if expecting == ExpectedNext.CONSEQUENT:
                            new_formula = Formula(formula_num, current_txt)
                            proof_step.consequents.append(new_formula)
                        else:
                            print(f'{line_number}: Was not expecting proof command')
                    current_txt = line[CI_LEN:]
                    command_parens = count_parens(current_txt)
                    inside_stmt = (command_parens > 0)
                    if not inside_stmt:
                        if not current_txt.startswith(POSTPONE_CMD):
                            proof_step.command = current_txt
                            expecting = ExpectedNext.ANTECEDENT
                            print('======')
                            print_proof_step(proof_step)
                            print('======')
                            results.append(proof_step)
                        proof_step = ProofStep([], [], "")
                elif not line.strip(): # blank line
                    if inside_stmt:
                        if expecting == ExpectedNext.CONSEQUENT:
                            new_formula = Formula(formula_num, current_txt)
                            proof_step.consequents.append(new_formula)
                            expecting = ExpectedNext.COMMAND
                        elif expecting == ExpectedNext.COMMAND:
                            command_parens += count_parens(current_txt)
                            if command_parens != 0:
                                print(f'{line_number}: Mismatched parentheses')
                            if not current_txt.startswith(POSTPONE_CMD):
                                proof_step.command = current_txt
                                expecting = ExpectedNext.ANTECEDENT
                                print('======')
                                print_proof_step(proof_step)
                                print('======')
                                results.append(proof_step)
                            proof_step = ProofStep([], [], "")
                        inside_stmt = False
                elif inside_stmt:
                    txt = line.strip()
                    current_txt = current_txt + " " + txt
                    if expecting == ExpectedNext.COMMAND:
                        command_parens += count_parens(txt)
                        inside_stmt = (command_parens > 0)
                        if not inside_stmt:
                            expecting = ExpectedNext.ANTECEDENT

build_proof_steps()
print('Done')
        
    
