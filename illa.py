import os
from itertools import permutations

def illa(nom_arxiu_entrada):
    # Check if the input file exists
    if not os.path.exists(nom_arxiu_entrada):
        print(f"File not found: {nom_arxiu_entrada}")
        return

    # Open the input file and read its lines
    lines = []
    with open(nom_arxiu_entrada, "r") as infile:
        for line in infile:
            lines.append(line.strip())

    # Build dictionaries for jealousies and friendships, and a list of boys
    jelouses = {}  # Who is jealous of who
    friends = {}   # Who is friends with who
    boys = []      # List of all boys
    for line in lines:
        name, jelous, friend = line.split()
        boys.append(name)
        jelouses[name] = jelous
        friends[name] = friend
    return boys, jelouses, friends


def evaluate(boys, jelouses, friends, root=None):
    # Find possible roots (nodes without cycles)
    possible_roots = found_cicle(jelouses, True)
    for root in possible_roots:
        # Build the graph and determine if it's possible to solve
        posible, graph, priority = build_graph(boys, jelouses, friends, root)
        if not posible:
            print("impossible")
            break
        else:
            # Generate a solution using DFS
            solution = dfs(graph, root, priority)
            #print(f'graph: {graph}')
            # Check if the solution is valid
            if not is_correct(solution, jelouses, friends):
                # If it's the last possible root and still invalid, print "impossible"
                if possible_roots.index(root) == len(possible_roots) - 1:
                    print("impossible")
                    break
                continue
            # Print the valid solution
            print_solution(solution)
            break


def build_graph(boys, jelouses, friends, root):
    # Create a graph excluding the root
    graph = create_graph(jelouses, root)

    # Get the jealous and friend relationships of the root
    jelous_root = jelouses[root]
    friend_root = friends[root]

    # Create paths for the jealous and friend relationships
    path_root_friend, unpriorited_path = create_paths(graph, jelous_root, friend_root)
    # Create a priority list for traversal
    priority = create_priority(root, unpriorited_path, friend_root, boys)

    # Check if the jealous person is in the path to the friend
    if jelous_root in path_root_friend:
        # Break cycles if necessary
        graph, posible = break_cicles(graph, root, friends)
    else:
        posible = True
    return posible, graph, priority


def print_solution(solution):
    # Format and print the solution
    solution = " ".join(solution)
    print(solution)


def create_priority(root, unpriorited_path, friend_root, boys):
    # Create a priority list for traversal
    priority = []
    priority.append(root)

    if friend_root not in priority:
        priority.append(friend_root)

    for node in boys:
        if node not in priority:
            if node not in unpriorited_path:
                priority.append(node)
    return priority


def create_paths(graph, jelous_root, friend_root):
    # Get paths for the jealous and friend relationships
    path_root_friend = path(graph, friend_root)
    unpriorited_path = path(graph, jelous_root)
    return path_root_friend, unpriorited_path


def what_is_the_roots(graph):
    # Find the nodes with the highest number of incoming edges
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
    # Create a copy of the jealousy dictionary and remove the root
    graph = jelouses.copy()
    del graph[root]
    return graph


def path(graph, node):
    # Follow the path from a node until it ends
    path = []
    current = node
    while current in graph:
        path.append(current)
        current = graph[current]
    return path


def break_cicles(graph, node, friends):
    # Break cycles in the graph by modifying edges
    friend_node = friends[node]
    graph[friend_node] = node

    seen_states = set()  # Track visited graph states

    exist_cicle = found_cicle(graph)

    while exist_cicle:
        node = friends[node]
        graph_state = str(graph)

        if graph_state in seen_states:
            return graph, False  # Cycle cannot be resolved
        seen_states.add(graph_state)

        del graph[node]  # Remove the problematic node

        graph[friends[friends[node]]] = friends[node]  # Redirect edges

        exist_cicle = found_cicle(graph)
    return graph, True


def found_cicle(graph, need_posible_roots=False):
    # Check if the graph contains a cycle
    visited = set()
    possible_roots = []

    for node in graph:
        if node in visited:
            continue
        # Follow the path from the node to detect cycles
        path = set()
        current = node

        while current in graph:
            if current in path:
                if need_posible_roots:
                    return possible_roots
                return True  # Cycle detected
            if current in visited:
                break
            path.add(current)
            visited.add(current)
            possible_roots.append(current)
            current = graph[current]
    return False  # No cycle found


def dfs(graph_origen, root, boys):
    # Perform a depth-first search (DFS) on the graph
    graph = {}

    # Initialize the graph structure
    for node in set(dict(graph_origen).keys() | set(dict(graph_origen).values())):
        graph[node] = []

    for node, father in dict(graph_origen).items():
        graph[father].append(node)

    # Define a priority function for sorting children
    def priority(node):
        if node in boys:
            return list(boys).index(node)
        return len(boys)

    # Sort children by priority
    for node, children in graph.items():
        graph[node] = sorted(children, key=priority, reverse=True)

    visiteds = set()
    stack = [root]
    path = []

    # Traverse the graph using DFS
    while stack:
        node = stack.pop()
        if node not in visiteds:
            visiteds.add(node)
            path.append(node)
            children = graph.get(node, [])
            for child in children:
                if child not in visiteds:
                    stack.append(child)
    return path


def is_correct(solucio, jelouses, friends):
    # Check if the solution satisfies the jealousy and friendship constraints
    posicions = {node: i for i, node in enumerate(solucio)}

    for node in solucio:
        jelous = jelouses[node]
        friend = friends[node]

        # Check if the jealous person appears after the node
        if jelous in posicions and posicions[node] < posicions[jelous]:
            # Check if the friend is not between the node and the jealous person
            if friend not in posicions or not (posicions[node] < posicions[friend] < posicions[jelous]):
                return False
    return True


if __name__ == "__main__":
    import sys

    # Read input and evaluate the solution
    boys, jelouses, friends = illa(sys.argv[1])
    evaluate(boys, jelouses, friends)