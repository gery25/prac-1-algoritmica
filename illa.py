#!/usr/bin/env python3
"""
Illa Problem Solver Module

This module implements a solution for the island problem, where boys have
jealousy and friendship relationships that need to be satisfied.
"""

import os
import sys
import time


class IllaProblemSolver:
    """Class to handle the island problem solving logic."""

    def __init__(self):
        """Initialize the solver with empty data structures."""
        self.boys_list = []
        self.jealousy_map = {}
        self.friendship_map = {}

    @staticmethod
    def time_decorator(function):
        """Decorator to measure function execution time."""
        def wrapper(*args, **kwargs):
            print("Starting DFS")
            start_time = time.time()
            result = function(*args, **kwargs)
            end_time = time.time()
            print(f'Time taken: {end_time - start_time} seconds')
            return result
        return wrapper

    def read_island_file(self, input_filename):
        """
        Read and parse the input file containing boy relationships.

        Args:
            input_filename (str): Path to the input file

        Returns:
            tuple: (boys_list, jealousy_map, friendship_map) or (None, None, None) on error
        """
        if not os.path.exists(input_filename):
            print(f"File not found: {input_filename}")
            return None, None, None

        try:
            with open(input_filename, "r", encoding='utf-8') as infile:
                input_lines = [line.strip() for line in infile]

            for line in input_lines:
                boy_name, jealous_of, friend = line.split()
                self.boys_list.append(boy_name)
                self.jealousy_map[boy_name] = jealous_of
                self.friendship_map[boy_name] = friend

            return self.boys_list, self.jealousy_map, self.friendship_map

        except (IOError, ValueError) as error:
            print(f"Error processing file: {error}")
            return None, None, None

    def evaluate_solution(self, output_file=None):
        """
        Find a valid solution that satisfies all constraints.

        Args:
            output_file (str, optional): Path to output file. Defaults to None.
        """
        possible_roots = self.find_cycle(self.jealousy_map, find_roots=True)

        for root_node in possible_roots:
            is_valid, graph, priority_list = self.build_graph(root_node)

            if not is_valid:
                continue

            solution = self.depth_first_search(graph, root_node, priority_list)

            if self.is_solution_valid(solution):
                self.print_solution(solution, output_file)
                return

        print("impossible")

    def build_graph(self, root_node):
        """
        Build a directed graph excluding the root node and establish relationships.

        Args:
            root_node (str): The root node to exclude

        Returns:
            tuple: (is_valid, graph, priority_list)
        """
        graph = self.create_graph_without_root(root_node)
        jealous_of_root = self.jealousy_map[root_node]
        friend_of_root = self.friendship_map[root_node]
        friend_path, jealous_path = self.find_paths(graph, jealous_of_root, friend_of_root)
        priority_list = self.create_priority_list(root_node, jealous_path, friend_of_root)

        if jealous_of_root in friend_path:
            graph, is_valid = self.break_cycles(graph, root_node)
        else:
            is_valid = True

        return is_valid, graph, priority_list

    @staticmethod
    def print_solution(solution, output_file=None):
        """
        Format and output the solution.

        Args:
            solution (list): List of boys in correct order
            output_file (str, optional): Path to output file. Defaults to None.
        """
        solution_str = " ".join(solution)
        if output_file is None:
            print(solution_str)
        else:
            try:
                with open(output_file, "w", encoding='utf-8') as outfile:
                    outfile.write(solution_str)
            except IOError as error:
                print(f"Error writing to file: {error}")

    def create_priority_list(self, root_node, jealous_path, friend_of_root):
        """
        Create a priority list for traversal.

        Args:
            root_node (str): The root node
            jealous_path (list): Path of jealousy relationships
            friend_of_root (str): Friend of the root node

        Returns:
            list: Priority list for traversal
        """
        priority_list = [root_node]

        if friend_of_root not in priority_list:
            priority_list.append(friend_of_root)

        for node in self.boys_list:
            if node not in priority_list and node not in jealous_path:
                priority_list.append(node)

        return priority_list

    def find_paths(self, graph, jealous_of_root, friend_of_root):
        """
        Create paths for the jealous and friend relationships.

        Args:
            graph (dict): The graph
            jealous_of_root (str): The node that is jealous of the root
            friend_of_root (str): The friend of the root

        Returns:
            tuple: (friend_path, jealous_path)
        """
        friend_path = self.path(graph, friend_of_root)
        jealous_path = self.path(graph, jealous_of_root)
        return friend_path, jealous_path

    def create_graph_without_root(self, root_node):
        """
        Create a copy of the jealousy dictionary and remove the root.

        Args:
            root_node (str): The root node to remove

        Returns:
            dict: The graph without the root node
        """
        graph = self.jealousy_map.copy()
        del graph[root_node]
        return graph

    @staticmethod
    def path(graph, node):
        """
        Follow the path from a node until it ends.

        Args:
            graph (dict): The graph
            node (str): The starting node

        Returns:
            list: The path from the node
        """
        path = []
        current = node
        while current in graph:
            path.append(current)
            current = graph[current]
        return path

    def break_cycles(self, graph, node):
        """
        Break cycles in the graph by modifying edges.

        Args:
            graph (dict): The graph
            node (str): The node to start breaking cycles from

        Returns:
            tuple: (graph, is_valid)
        """
        friend_node = self.friendship_map[node]
        graph[friend_node] = node
        seen_states = set()
        exist_cycle = self.find_cycle(graph)

        while exist_cycle:
            node = self.friendship_map[node]
            graph_state = str(graph)

            if graph_state in seen_states:
                return graph, False

            seen_states.add(graph_state)
            del graph[node]
            graph[self.friendship_map[self.friendship_map[node]]] = self.friendship_map[node]
            exist_cycle = self.find_cycle(graph)

        return graph, True

    @staticmethod
    def find_cycle(graph, find_roots=False):
        """
        Check if the graph contains a cycle.

        Args:
            graph (dict): The graph
            find_roots (bool, optional): Whether to find possible root nodes. 
            Defaults to False.

        Returns:
            bool or list: True if cycle found, False otherwise, 
            or list of possible roots if find_roots is True
        """
        visited = set()
        possible_roots = []

        for node in graph:
            if node in visited:
                continue

            path = set()
            current = node

            while current in graph:
                if current in path:
                    if find_roots:
                        return possible_roots
                    return True

                if current in visited:
                    break

                path.add(current)
                visited.add(current)
                possible_roots.append(current)
                current = graph[current]

        return False

    @staticmethod
    def depth_first_search(graph_origen, root, boys):
        """
        Perform a depth-first search (DFS) on the graph.

        Args:
            graph_origen (dict): The original graph
            root (str): The root node
            boys (list): List of boys

        Returns:
            list: The DFS traversal path
        """
        graph = {}

        for node in set(dict(graph_origen).keys() | set(dict(graph_origen).values())):
            graph[node] = []

        for node, father in dict(graph_origen).items():
            graph[father].append(node)

        def priority(node):
            if node in boys:
                return list(boys).index(node)
            return len(boys)

        for node, children in graph.items():
            graph[node] = sorted(children, key=priority, reverse=True)

        visiteds = set()
        stack = [root]
        path = []

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

    def is_solution_valid(self, solution):
        """
        Check if the solution satisfies the jealousy and friendship constraints.

        Args:
            solution (list): The solution path

        Returns:
            bool: True if valid, False otherwise
        """
        positions = {node: i for i, node in enumerate(solution)}

        for node in solution:
            jealous_of = self.jealousy_map[node]
            friend = self.friendship_map[node]

            if jealous_of in positions and positions[node] < positions[jealous_of]:
                is_friend_between = positions[node] < positions[friend] < positions[jealous_of]
                if friend not in positions or not is_friend_between:
                    return False

        return True

    @staticmethod
    #@time_decorator
    def read_console_input():
        """
        Read input from the command line.

        Returns:
            tuple: (boys_list, jealousy_map, friendship_map)
        """
        print("Enter data (format: name jealous friend). Ctrl+D to finish:")
        lines = []

        try:
            while True:
                line = input().strip()
                if line:
                    parts = line.split()
                    if len(parts) != 3:
                        print("Error: Each line must have 3 names separated by spaces")
                        continue
                    lines.append(line)
        except EOFError:
            pass

        jealousy_map = {}
        friendship_map = {}
        boys_list = []

        for line in lines:
            boy_name, jealous_of, friend = line.split()
            boys_list.append(boy_name)
            jealousy_map[boy_name] = jealous_of
            friendship_map[boy_name] = friend

        return boys_list, jealousy_map, friendship_map


def main():
    """Main entry point of the program."""
    solver = IllaProblemSolver()

    if len(sys.argv) == 1:
        boys, _, _ = solver.read_console_input()
        if boys:
            solver.evaluate_solution()

    elif len(sys.argv) == 2:
        boys, _, _ = solver.read_island_file(sys.argv[1])
        if boys:
            solver.evaluate_solution()

    elif len(sys.argv) == 3:
        boys, _, _ = solver.read_island_file(sys.argv[1])
        if boys:
            solver.evaluate_solution(sys.argv[2])

    else:
        print("Usage:")
        print("1. Console input: python3 illa.py")
        print("2. Input file: python3 illa.py input.txt")
        print("3. Input and output: python3 illa.py input.txt output.txt")


if __name__ == "__main__":
    main()
