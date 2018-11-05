from pathlib import Path
from typing import List
import subprocess

def running() -> List[str]:
    b_sessions, errors = subprocess.Popen(
            ['tmux', 'list-sessions'],
            stderr=subprocess.PIPE,
            stdout=subprocess.PIPE).communicate()

    if errors:
        print(errors.decode())
        return []

    sessions = b_sessions.decode().strip().split('\n')
    return [s.split(':')[0] + ' running' for s in sessions]


def static(tmuxp_path: Path) -> List[str]:
    ext = '.yaml'
    sessions = tmuxp_path.glob(fr'*{ext}')
    return [s.name.replace(ext, '') + ' static' for s in sessions]


def make_cmd(choice: str) -> str:
    arg = ''
    if choice.strip().endswith(' static'):
        arg = choice.strip().replace(' static', '')
        cmd = f'tmuxp load {arg}'
    elif choice.strip().endswith(' running'):
        arg = choice.strip().replace(' running', '')
        cmd = f'tmux attach -t {arg}'
    return cmd


def combine(running: List[str], static: List[str]) -> List[str]:
    r = [n.replace(' running', '') for n in running]

    for s in static:
        if s.replace(' static', '') not in r:
            running.append(s)

    return running   


if __name__ == '__main__':
    st = static(Path('~/.tmuxp').expanduser())
    rn = running()

    combined = combine(rn, st)

    sessions = '\n'.join(combined)

    fzf = subprocess.Popen(
            ['fzf', '-1', '-0'],
            stdout=subprocess.PIPE,
            stdin=subprocess.PIPE,
            shell=True)
    choice, _ = fzf.communicate(sessions.encode())
    print(make_cmd(choice.decode()))
