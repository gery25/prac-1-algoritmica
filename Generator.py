import os
import random
import string
from itertools import permutations

def generar_exemples(inici=4, fi=10, num_exemples=20):
    # Crear carpetes
    carpeta_entrada = "exemples"
    carpeta_sortida = "solucions"
    os.makedirs(carpeta_entrada, exist_ok=True)
    os.makedirs(carpeta_sortida, exist_ok=True)

    # Generar arxius de 4 a 15 nois, 10 exemples cadascun
    for n_nois in range(inici, fi):
        print("Generant exemples amb", n_nois, "nois")
        for i in range(1, num_exemples+1):
            nom_arxiu_entrada = os.path.join(carpeta_entrada, f"illa-{n_nois}-{i}.txt")
            nom_arxiu_sortida = os.path.join(carpeta_sortida, f"solucio-{n_nois}-{i}.txt")

            # Generar noms de nois
            noms_nois = random.sample(string.ascii_uppercase, n_nois)

            with open(nom_arxiu_entrada, "w") as f:
                for idx, noi in enumerate(noms_nois):
                    possibles_enemic = [x for x in noms_nois[:idx]] if idx > 0 else [x for x in noms_nois if x != noi]
                    enemic = random.choice(possibles_enemic)
                    possibles_amic = [x for x in noms_nois if x != noi and x != enemic]
                    amic = random.choice(possibles_amic)
                    f.write(f"{noi} {enemic} {amic}\n")

            # Obrir el firxer d'entrada
            lines = []
            with open(nom_arxiu_entrada, 'r') as infile:
                for line in infile:
                    lines.append(line.strip())

            # Construeix el diccionari de relacions
            relations = {}
            for line in lines:
                name, jelous, friend = line.split()
                relations[name] = (jelous, friend)

            # Genera permutacions vàlides
            valid_permutations = []
            for perm in permutations(list(relations.keys())):
                valid = True
                for i, name in enumerate(perm):
                    jelous, friend = relations[name]
                    friend_pos = perm.index(friend)
                    jelous_pos = perm.index(jelous)
                    if jelous_pos > i and (friend_pos > jelous_pos or friend_pos < i):
                        valid = False
                        break
                if valid:
                    valid_permutations.append(' '.join(perm))

            with open(nom_arxiu_sortida, 'w') as outfile:
                if len(valid_permutations) == 0:
                    outfile.write("impossible")
                else:
                    # Escriu les solucions a l'arxiu de sortida
                    outfile.write('\n'.join(valid_permutations))

if __name__ == "__main__":
    import sys
    if len(sys.argv) > 1:
        generar_exemples(int(sys.argv[1]), int(sys.argv[2])+1)
    else:
        generar_exemples()
