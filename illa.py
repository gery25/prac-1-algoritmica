import os
from itertools import permutations

def illa(nom_arxiu_entrada):
    # Comprova si l'arxiu d'entrada existeix
    if not os.path.exists(nom_arxiu_entrada):
        print(f"Arxiu no trobat: {nom_arxiu_entrada}")
        return

    # Obrir el fitxer d'entrada
    lines = []
    with open(nom_arxiu_entrada, "r") as infile:
        for line in infile:
            lines.append(line.strip())

    # Construeix el diccionari de relacions
    relations = {}
    jelouses = {}
    friends = {}
    boys = []
    for line in lines:

        name, jelous, friend = line.split()
        boys.append(name)
        jelouses[name] = jelous  #  qui ens eniemig de qui
        friends[name] = friend  # qui es amig de qui
        relations[name] = (jelous, friend)
    #print("jelouses: ", jelouses)
    #print("friends: ", friends)
    return boys, jelouses, friends
    
    

def evaluate(boys, jelouses, friends, root=None):

    #print("firs jelouses evaluate: ", jelouses)

    #list_equals_sum_root = what_is_the_roots(jelouses)  # agafem el root

    possible_roots = found_cicle(jelouses, True)  # agafem el root
    print("list_equals_sum_root: ", possible_roots)
    for root in possible_roots:
        print("root: ", root)
        posible, graph, priority = build_graph(boys, jelouses, friends, root)  # creem el graf i agafem el root
        #print("hola")
        #print("posible: ", posible)
        if not posible:  # si no es possible
            print("impossible1")
            break
        else:
            print("graph: ", graph)
            solution = dfs(graph, root, priority)   # creem la solucio amb l'algoritme dfs
            #print("jelouses before is_correct: ", jelouses)
            print("solution: ", solution)
            #print("possible1")
            if not is_correct(solution, jelouses, friends):  # si no es possible
                if possible_roots.index(root) == len(possible_roots) :  # si es el ultim root 
                    print("impossible2")
                    break   
                continue
            
            #print("possible")
            print_solution(solution)  # printem la solucio
            break



def build_graph(boys, jelouses, friends, root):
    #print("jelouses build graph: ", jelouses)
    graph = create_graph(jelouses, root)
    #print("jelouses build graph after crate graph: ", jelouses)
    jelous_root = jelouses[root]  # agafem el primer amic
    friend_root = friends[root]  # agafem l'amic del root
    
    path_root_friend, unpriorited_path = create_paths(graph, jelous_root, friend_root)  # agafem el cami de l'amic

    priority = create_priority(root, unpriorited_path, friend_root, boys)  # creem la llista de prioritat

    if jelous_root in path_root_friend:  # si l'amic es el root
        #print("l'amic es el root")
        #print(graph)
        graph, posible = break_cicles(graph, root, friends)  # posem a l'amic entre els dos
        #print(posible)
        #print(graph)
    else:
        posible = True  # si no es possible, no es possible
    return posible, graph, priority  # tornem el graf i el root


def print_solution(solution): 
    # Fica la solucio en el format que toca
    solution = " ".join(solution)
    print(solution)  # printem la solucio

def create_priority(root, unpriorited_path, friend_root, boys):
    priority = []
    priority.append(root)  # afegim el root a la llista de prioritat
    #priority.append(friends[root])  # afegim l'amic a la llista de prioritat

    #for node in list_equals_sum_root:
    #    if node not in priority:
    #        if node not in unpriorited_path:
    #            priority.append(node)

    #print("priority: ",priority)
    #print("list of nodes: ",list_equals_sum_root)

    
    if friend_root not in priority:
        priority.append(friend_root)  # afegim l'amic a la llista de prioritat

    #print("priority: ", priority)
    #print("list of nodes: ", list_equals_sum_root)

    for node in boys:
        if node not in priority:  # si no es el root ni l'amic
            if node not in unpriorited_path:  # si no es el cami de l'enemic
                priority.append(node)
    print("priority: ", priority)

    return priority

def create_paths(graph, jelous_root, friend_root):
    

    path_root_friend = path(graph, friend_root)  # agafem el cami de l'amic

    #print("path root friend: ", path_root_friend)

    unpriorited_path = path(graph, jelous_root)  

    return path_root_friend, unpriorited_path

def what_is_the_roots(graph):
    list_of_nodes = []
    max_sum = 0
    for node_check in graph:
        node_sum = 0
        for node_compare in graph:
           if node_check == graph[node_compare]:
                node_sum += 1
        if node_sum > max_sum:
            max_sum = node_sum
        if node_sum == max_sum:
            list_of_nodes.append(node_check)

    return list_of_nodes

def create_graph(jelouses, root):
    #print("create graph: ", jelouses)
    graph = jelouses.copy()  # fem una copia del diccionari
    #print("jelouses first create graph: ", jelouses)
    
    #root, list_equals_root = what_is_the_root(graph, root)
    #print("root: ",root)
    #print("graph create graph: ",graph)
    #print("jelouses create graph: ", jelouses)
    del graph[root]

    #print("graph after create graph : ", graph)
    #print ("jelouses after create graph: ", jelouses)
    #print("graph: ",graph)
    return graph #, root #, list_equals_root

def path(graph, node):
    path = []
    current = node
    while current in graph:
        path.append(current)
        current = graph[current]
    return path

def break_cicles(graph, node, friends):

    friend_node = friends[node]  # agafem l'amic del node
    graph[friend_node] =  node  # posem a l'amic entre els dos
    #print("graph: ", graph)

    seen_states = set()  # Set to keep track of visited nodes

    exist_cicle = found_cicle(graph)
    print("exist_cicle: ", exist_cicle)

    while exist_cicle:  # If exist a cycle

        node = friends[node]
        graph_state = str(graph)  # Convert the graph to a string representation

        #print("graph_state: ", graph_state)

        if graph_state in seen_states:  # Check if the current state has been seen before
            #print("Cycle detected in graph: ", graph)
            return graph, False  # If it has, set posible to False
        
        seen_states.add(graph_state)  # Add the current state to the set of seen states
        del graph[node]  # Remove the node that is causing the cycle
        graph[friends[friends[node]]] = friends[node] # change the edge of friend of the node to friend of the friend of the node
        #print("graph: ",graph)
        exist_cicle = found_cicle(graph)  # Check for cycles again
        print("exist_cicle: ", exist_cicle)
    return graph, True


# this functions searche in the graf if is a cicle or not and return a boolean
def found_cicle(graph, need_posible_roots = False):
    visited = set()  # molt més eficient per cerca
    possible_roots = []

    for node in graph:
        if node in visited:
            continue

        # Seguim el camí des del node fins trobar un cicle o acabar
        path = set()
        current = node

        while current in graph:
            if current in path:
                if need_posible_roots:
                    return possible_roots
                return True  # hem trobat un cicle
            if current in visited:
                break
            path.add(current)
            visited.add(current)
            possible_roots.append(current)
            current = graph[current]
    return False # no s'ha trobat cap cicle


def dfs(graph_origen, root, boys):
    graph = {}

    for node in set(dict(graph_origen).keys() | set(dict(graph_origen).values())):
        graph[node] = []
    #print("graph: ",graph)

    for node, father in dict(graph_origen).items():
        graph[father].append(node)  # afegim el node al seu pare

    #print("graf dfs: ",graph)
    def priority(node):
        if node in boys:
            return list(boys).index(node)
        return len(boys)
    
    for node, children in graph.items():
       graph[node] = sorted(children, key=priority, reverse=True)  # ordenem els fills per la seva prioritat
       #print("children: ",graph[node])
    #print("graph amb prioritats: ",graph)
        

    visiteds = set()
    stack = [root]
    path = []
    
    while stack:
        node = stack.pop()

        if node not in visiteds:
            visiteds.add(node)
            path.append(node)

            children = graph.get(node, [])
            for child in (children):
                if child not in visiteds:
                    stack.append(child)
    return path


def is_correct(solucio, jelouses, friends):
# Creem un diccionari per obtenir la posició de cada node en la solució
    posicions = {node: i for i, node in enumerate(solucio)}
    print("posicions: ", posicions)
    for node in solucio:
        #print("jelouses: ",jelouses)
        #print("friends: ",friends)
        jelous = jelouses[node]
        friend = friends[node]

        # Comprova si l'enemic apareix després del node
        if jelous in posicions and posicions[node] < posicions[jelous]:
            # Comprova si l'amic no està entre el node i l'enemic
            if friend not in posicions or not (posicions[node] < posicions[friend] < posicions[jelous]):
                return False
    return True

if __name__ == "__main__":
    import sys

    boys, jelouses, friends = illa(sys.argv[1])
    evaluate(boys, jelouses, friends)