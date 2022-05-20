import subprocess, time, os.path, filecmp

# THIS SCRIPT IS CURRENTLY ONLY GOOD FOR PROLOG
# PLEASE REVIEW BEFORE USING IT FOR ANOTHER LANGUAGE

swi_prolog_executable = "swi-prolog.swipl"
couladj_prolog = "CoulAdj.pl"

pixels_directory = "tests/knowledge-bases/"
result_directory = "tests/results/"
golden_results = "tests/golden.tsv"

print("Benchmark started")

def pixels_in_size(size):
    # see https://github.com/AmeliaSZK/CoulAdj-TestSamples#sizes
    nb_pixels = {
       "1" : 72,
       "2" : 288,
       "4" : 1152,
       "8" : 4608,
       "16" : 18432,
       "32" : 73728,
       "64" : 294912,
       "128" : 1179648,
       "256" : 4718592,
       "512" : 18874368
    }
    return nb_pixels[str(size)]

def test_one_size(size):
    pixels_filename = f"pixels-size-{size}.pl"
    result_filename = f"result-size-{size}.tsv"
    pixels_path = os.path.join(pixels_directory, pixels_filename)
    result_path = os.path.join(result_directory, result_filename)

    if not os.path.exists(pixels_path):
        print(f"Data for size {size} is not in the repo; this file doesn't exist: {pixels_path}")
        return

    start = time.perf_counter()
    subprocess.run(
        [swi_prolog_executable, 
        *["-O", "--no-debug"],
        couladj_prolog, 
        pixels_path, 
        result_path]).check_returncode()
    end = time.perf_counter()
    duration = round(end - start, 3)

    nb_pixels = pixels_in_size(size)
    pixels_per_second = round(nb_pixels / duration)

    is_correct = filecmp.cmp(golden_results, result_path, shallow=False)
    correctness_msg = "Correct" if is_correct else "INCORRECT"
    print(f"{duration}", f"Size {size}", correctness_msg, f"{pixels_per_second} pixels/s", sep="\t")

for size in [1,2,4,8]:
    test_one_size(size)





