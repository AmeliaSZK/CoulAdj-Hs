import subprocess, time, os.path, filecmp

executable = "cabal exec"
couladj = "CoulAdj-Hs"

images_directory = "tests\\"
result_directory = "tests\\results\\"
golden_results = "tests\\golden.tsv"

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
    image_filename = f"sample-size-{size}.png"
    result_filename = f"result-size-{size}.tsv"
    image_path = os.path.join(images_directory, image_filename)
    result_path = os.path.join(result_directory, result_filename)

    if not os.path.exists(image_path):
        print(f"Data for size {size} is not in the repo; this file doesn't exist: {image_path}")
        return
    
    #print(f"image_path = {image_path}")
    #print(f"result_path = {result_path}")

    cmdline = f"{executable} {couladj} {image_path} {result_path}"
    #print(cmdline)

    start = time.perf_counter()
    # subprocess.run(
    #     [executable, 
    #     #*["-O", "--no-debug"],
    #     couladj, 
    #     image_path, 
    #     result_path]).check_returncode()
    subprocess.run(cmdline, shell=True).check_returncode()
    end = time.perf_counter()
    duration = round(end - start, 3)

    nb_pixels = pixels_in_size(size)
    pixels_per_second = round(nb_pixels / duration)

    is_correct = filecmp.cmp(golden_results, result_path, shallow=False)
    correctness_msg = "Correct" if is_correct else "INCORRECT"
    print(f"{duration}", f"Size {size}", correctness_msg, f"{pixels_per_second} pixels/s", sep="\t")

for size in [1,2,4,8]:
    test_one_size(size)





