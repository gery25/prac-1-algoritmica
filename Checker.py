import os
import subprocess
import time

matplotlib_installed = False
try:
    import matplotlib.pyplot as plt
    matplotlib_installed = True
except:
    print("Please install matplotlib")

debug = False

def chequejar_exemples(executable, inici = 4, fi = 16, num_exemples=20):
    # Directori actual (pot canviar-se si cal)
    directory = os.getcwd()
    input_directory = os.path.join(directory, 'exemples')
    output_directory = os.path.join(directory, 'solucions')
    os.makedirs(output_directory, exist_ok=True)

    # Comanda per executar el programa
    if executable.endswith(".py"):
        command_line = ['python3', executable]
    else:
        command_line = [executable]


    # Itera sobre els valors de n i m
    times = []
    for n in range(inici, fi):
        print(f"Chequejant exemples amb {n} nois")
        point_time = 0
        for m in range(1, num_exemples+1):
            input_filename = os.path.join(input_directory, f"illa-{n}-{m}.txt")
            output_filename = os.path.join(output_directory, f"solucio-{n}-{m}.txt")

            # Comprova si l'arxiu d'entrada existeix
            if not os.path.exists(input_filename):
                print(f"Arxiu no trobat: {input_filename}")
                raise FileNotFoundError

            if debug:
                print(f"Testing {input_filename}")

            # Executa el programa
            initial_time = time.time()
            process = subprocess.run(
                command_line + [input_filename],
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
            )
            final_time = time.time()
            output_data = process.stdout.decode("utf-8")
            error_data = process.stderr.decode("utf-8")
            with open("output.txt", "w") as temp_file:
                temp_file.write(output_data + error_data)

            solution = output_data.split()
            point_time += final_time - initial_time

            # Comprova si l'arxiu de sortida existeix
            if debug:
                print(f"Checking {output_filename}")
            if not os.path.exists(output_filename):
                print(f"Arxiu no trobat: {output_filename}")
                raise FileNotFoundError

            # Llegeix les solucions de l'arxiu de sortida
            solucions = []
            with open(output_filename, 'r') as f:
                for line in f:
                    solucions.append(line.split())

            if debug:
                # Compara les solucions
                print("Solucions:", solucions)
                print("Output:", solution)
            if solution not in solucions:
                print(f"Error en el cas {input_filename}")
                print(f"Output: {solution}")
                print(f"Solucions: {solucions}")
                raise ValueError

        times.append( (n, point_time) )

    return times

def print_times(inici, fi, times, executable):
    # Print the times
    if matplotlib_installed:
        plt.legend(loc="upper left")
        plt.xticks(range(inici, fi + 1))
        plt.title("Time to solve the problem:" + executable)
        plt.xlabel("Number of boys")
        plt.ylabel("Time (s)")
        plt.plot(*zip(*times))
        plt.savefig("time.png")
        plt.show()
    else:
        print(times)    

if __name__ == '__main__':
    import sys

    times = []
    inici, fi = 0, 0
    executable = sys.argv[1]
    if len(sys.argv) > 2:
        inici = int(sys.argv[2])
        fi = int(sys.argv[3])
        times = chequejar_exemples(executable, inici, fi+1)
    else:
        inici = 4
        fi = 15
        times = chequejar_exemples(executable, inici, fi+1)

    print_times(inici, fi, times, executable)
