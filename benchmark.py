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
    nb_pixels = pixels_in_size(size)
    img_name = f"Size {size}"
    test_one_image(img_name, nb_pixels, image_filename, result_filename, golden_results)
    

def test_one_image(img_name, nb_pixels, image_filename, result_filename, golden):
    image_path = os.path.join(images_directory, image_filename)
    result_path = os.path.join(result_directory, result_filename)

    if not os.path.exists(image_path):
        print(f"Image file note found: {image_path}")
        return

    cmdline = f"{executable} {couladj} {image_path} {result_path}"
    #print(cmdline)

    start = time.perf_counter()
    subprocess.run(cmdline, shell=True).check_returncode()
    end = time.perf_counter()
    duration = round(end - start, 3)

    pixels_per_second = round(nb_pixels / duration)

    is_correct = filecmp.cmp(golden, result_path, shallow=False)
    correctness_msg = "Correct" if is_correct else "INCORRECT"
    print(f"{duration}", f"{img_name}", correctness_msg, f"{pixels_per_second} pixels/s", sep="\t")

test_one_image(
    "MortEmp", 
    3352*4096, 
    "proprietary\\warhammer_map_1_1_shadow.bmp",
    "result-shadow.tsv", 
    "tests\\proprietary\\golden warhammer_map_1_1_shadow.tsv"
    )

# for size in [1,2,4,8,16,32,64,128,256,512]:
#     test_one_size(size)





