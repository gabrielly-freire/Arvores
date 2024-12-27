//
// Criado por Vitor em 10/12/24.
//

#include <iostream>
#include <string>
#include <sstream>
#include <vector>

#include "Tree.h"
#include "SearchBinTree/SearchBinTree.h"

/**
 * Esta função divide a string de entrada digitada pelo usuário e a converte
 * em um par <comando, valor>
 *
 * @param input A string de comando digitada pelo usuário.
 * @return Um par com a chave (flag do comando) e o valor.
 */
std::pair<std::string, std::string> splitCommand(const std::string& input) {
  std::istringstream iss(input);
  std::string key;
  std::string value;

  iss >> key;
  iss >> value;

  return {key, value};
}

int main() {
  std::string cliUser;                          //<- Input user.
  TreeNode *root = nullptr;                     //<- A TreeNode to root.

  /** Uncomment this line to add nodes default. */
    root = insertNode(root, 22);
    root = insertNode(root, 23);
    root = insertNode(root, 30);
    root = insertNode(root, 21);
    root = insertNode(root, 18);
    root = insertNode(root, 17);
    root = insertNode(root, 10);
    root = insertNode(root, 15);

  while (cliUser != "exit") {
    std::cout << ">> ";
    std::getline(std::cin, cliUser);

    auto [key, value] = splitCommand(cliUser);

    if (key == "--help") {
      std::cout << "--help   : see helper. \n";
      std::cout << "--s      : show tree\n";
      std::cout << "--s-in   : show tree in order\n";
      std::cout << "--s-pre  : show tree pre-order\n";
      std::cout << "--s-pos  : show tree pos-order\n";
      std::cout << "--s-lev  : show tree level-order\n";
      std::cout << "--i      : insert node in tree\n";
      std::cout << "           --i [value]\n";
      std::cout << "--d      : delete node in tree\n";
      std::cout << "           --d [value]\n";
      std::cout << "--f      : check if node is in tree\n";
      std::cout << "           --f [value]\n";
      std::cout << "--is-bin: check i f tree is binary tree\n";
    }

    // Mostra a árvore.
    if (key == "--s") {
      printTree(root);
      std::cout << std::endl;
    }

    // Mostra a árvore seguindo o percursso de ordem.
    if (key == "--s-in") {
      inOrder(root);
      std::cout << std::endl;
    }

    // Mostra a árvore seguindo o percusso de pré-ordem.
    if (key == "--s-pre") {
      preOrder(root);
      std::cout << std::endl;
    }

    // Mostra a árvore seguindo o percusso de pós-ordem.
    if (key == "--s-pos") {
      postOrder(root);
      std::cout << std::endl;
    }

    // Mostra a árvore seguindo o percusso de nível.
    if (key == "--s-lev") {
      levelOrder(root);
      std::cout << std::endl;
    }

    // Insere um novo nó.
    if (key == "--i") { root = insertNode(root, stoi(value)); }

    // Deleta um nó.
    if (key == "--d") { root = deleteNode(root, stoi(value)); }

    // Encontra um nó na árvore.
    if (key == "--f") {
      if (search(root, stoi(value)) == true) {
        std::cout << "True\n";
      } else {
        std::cout << "False\n";
      }
    }

    // Checa se a árvore é binária.
    if (key == "--is-bin") {
      if (isBinarySearchTree(root)) {
        std::cout << "True\n";
      } else {
        std::cout << "False\n";
      }
    }
  }

  return 0;
}
