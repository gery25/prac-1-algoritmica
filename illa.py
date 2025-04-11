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
    
    
    graph, root, jelous_root, list_equals_sum_root = create_graph(jelouses)
    friend_root = friends[root]  # agafem l'amic del root


    path_root_friend = path(graph, friend_root)  # agafem el cami de l'amic

    #print("path root friend: ",path_root_friend)

    unpriorited_path = path(graph, jelous_root)  

    priority = []
    priority.append(root)  # afegim el root a la llista de prioritat
    #priority.append(friends[root])  # afegim l'amic a la llista de prioritat

    for node in list_equals_sum_root:
        if node not in priority:
            if node not in unpriorited_path:
                priority.append(node)

    #print("priority: ",priority)
    #print("list of nodes: ",list_equals_sum_root)

    if friend_root not in list_equals_sum_root: 
        if friend_root not in priority:
            priority.append(friend_root)  # afegim l'amic a la llista de prioritat

    #print("priority: ",priority)
    #print("list of nodes: ",list_equals_sum_root)

    for node in boys:
        if node not in priority:  # si no es el root ni l'amic
            if node not in unpriorited_path:  # si no es el cami de l'enemic
                priority.append(node)
    #print("priority: ",priority)




    if jelous_root in path_root_friend:  # si l'amic es el root
        #print("l'amic es el root")
        #print(graph)
        graph, posible = break_cicles(graph, root, friends)  # posem a l'amic entre els dos
        #print(posible)
        #print(graph)
    else:
        posible = True  # si no es possible, no es possible

    if not posible:  # si no es possible
        print("impossible")
    else:
        solution = dfs(graph, root, priority)   # creem la solucio amb l'algoritme dfs

        # fica la solucio en el format que toca
        solution = " ".join(solution)

        print(solution)     # printem la solucio

def what_is_the_root(graph):
    max = ''
    list_of_nodes = []
    max_sum = 0
    for node_check in graph:
        node_sum = 0
        for node_compare in graph:
           if node_check == graph[node_compare]:
                node_sum += 1
        if node_sum > max_sum:
            max_sum = node_sum
            max = node_check
        if node_sum == max_sum and max != node_check:
            list_of_nodes.append(node_check)
    return max, list_of_nodes

def create_graph(graph):
    root, list_equals_root = what_is_the_root(graph)
    jelous = graph[root]
    #print("root: ",root)
    #print("graph: ",graph)
    del graph[root]
    #print("graph: ",graph)
    return graph, root, jelous, list_equals_root

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
    
    seen_states = set()  # Set to keep track of visited nodes

    exist_cicle = found_cicle(graph)

    while exist_cicle:  # If exist a cycle
        node = friends[node]
        graph_state = str(graph)  # Convert the graph to a string representation

        if graph_state in seen_states:  # Check if the current state has been seen before
            return graph, False  # If it has, set posible to False
        
        seen_states.add(graph_state)  # Add the current state to the set of seen states
        del graph[node]  # Remove the node that is causing the cycle
        graph[friends[friends[node]]] = friends[node] # change the edge of friend of the node to friend of the friend of the node
        #print("graph: ",graph)
        exist_cicle = found_cicle(graph)  # Check for cycles again
    return graph, True


# this functions searche in the graf if is a cicle or not and return a boolean
def found_cicle(graph):
    visited = set()  # molt més eficient per cerca
    for node in graph:
        if node in visited:
            continue

        # Seguim el camí des del node fins trobar un cicle o acabar
        path = set()
        current = node

        while current in graph:
            if current in path:
                return True  # hem trobat un cicle
            if current in visited:
                break  # ja hem visitat aquesta branca, no hi ha cicle

            path.add(current)
            visited.add(current)
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

if __name__ == "__main__":
    import sys

    illa(sys.argv[1])