import os
import subprocess

files = []

def generate_res():
  fs = []
  for f in os.listdir('grading'):
    if f[-7:] == ".cl.out":
      fs.append(f[:-7])
  for file in fs:
    subprocess.run(f"coolc grading/{file}.cl", shell=True)
    subprocess.run(f'spim grading/{file}.s > grading/{file}.out', shell=True, capture_output=True)
    subprocess.run(f"rm grading/{file}.s", shell=True)
generate_res()

for file in os.listdir('grading'):
  if file[-7:] == ".cl.out":
    files.append(file[:-7])
files.sort()
# print(files)

out_dir = 'test-output'
if os.path.isdir(out_dir):
  subprocess.run(f"rm -rf {out_dir}/*", shell=True)
else:
  os.mkdir(out_dir)

def test():
  correct_test = 0
  for file in files:
    assembly_file = file + '.s'
    subprocess.run(f"./mycoolc grading/{file}.cl", shell=True)
    subprocess.run(f'spim grading/{assembly_file} > {out_dir}/{file}.out', shell=True, capture_output=True)
    print(f'Judging {file}.cl:')
    test_res = subprocess.run(f'diff grading/{file}.out {out_dir}/{file}.out', shell=True, capture_output=True)
    if (test_res.stdout == b''):
      print(f'\t{file}.cl correct!')
      subprocess.run(f'rm {out_dir}/{file}.out', shell=True)
      correct_test += 1
    else:
      print(f'\t{file}.cl fail!')
      subprocess.run(f'cp grading/{file}.cl {out_dir}/{file}.cl', shell=True)
      subprocess.run(f'cp grading/{file}.s {out_dir}/{file}.s', shell=True)
      subprocess.run(f'diff grading/{file}.out {out_dir}/{file}.out > {out_dir}/{file}.diff', shell=True, capture_output=True)
  print(f'PASS {correct_test}/{len(files)} TEST!')
test()
