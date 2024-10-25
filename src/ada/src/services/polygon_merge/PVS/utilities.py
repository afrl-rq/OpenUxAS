import os

def count_parens(txt):
    count = 0
    for c in txt:
        if c == '(':
            count += 1
        elif c == ')':
            count -= 1
    return count

def find_PVS_log_files():
    log_files = []
    for p, d, f in os.walk('.'):
        for file in f:
            if file.endswith('.log'):
                log_files.append(file)
    return log_files

