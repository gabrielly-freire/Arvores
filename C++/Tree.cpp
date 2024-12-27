//
// Criado por Vitor em 07/12/24.
//

#include "Tree.h"

#include <iostream>
#include <queue>

using namespace std;

/**
 * Construtor da classe TreeNode.
 * Inicializa o valor do nó e define os ponteiros dos filhos esquerdo e direito como nullptr.
 */
TreeNode::TreeNode(int value) : value(value), leftSon(nullptr), rightSon(nullptr) {}

/**
 * Realiza uma travessia em pré-ordem da árvore binária.
 * Visita o nó atual primeiro, depois a subárvore esquerda e finalmente a subárvore direita.
 * @param root Ponteiro para a raiz da árvore.
 */
void preOrder(TreeNode* root) {
  std::cout << root->value << " ";
  if (root->leftSon != nullptr) { preOrder(root->leftSon); }
  if (root->rightSon != nullptr) { preOrder(root->rightSon); }
}

/**
 * Realiza uma travessia em ordem (in-order) da árvore binária.
 * Visita a subárvore esquerda primeiro, depois o nó atual e finalmente a subárvore direita.
 * @param root Ponteiro para a raiz da árvore.
 */
void inOrder(TreeNode* root) {
  if (root->leftSon != nullptr) { inOrder(root->leftSon); }
  std::cout << root->value << " ";
  if (root->rightSon != nullptr) { inOrder(root->rightSon); }
}

/**
 * Realiza uma travessia em pós-ordem (post-order) da árvore binária.
 * Visita a subárvore esquerda primeiro, depois a subárvore direita e finalmente o nó atual.
 * @param root Ponteiro para a raiz da árvore.
 */
void postOrder(TreeNode* root) {
  if (root->leftSon != nullptr) { postOrder(root->leftSon); }
  if (root->rightSon != nullptr) { postOrder(root->rightSon); }
  std::cout << root->value << " ";
}

/**
 * Realiza uma travessia em nível (level-order) da árvore binária.
 * Visita os nós nível por nível, começando pela raiz.
 * @param root Ponteiro para a raiz da árvore.
 */
void levelOrder(TreeNode* root) {
  if (root == nullptr) { return; }

  queue<TreeNode*> q;
  q.push(root);

  while (!q.empty()) {
    TreeNode* node = q.front();
    q.pop();

    std::cout << node->value << " ";

    if (node->leftSon != nullptr) { q.push(node->leftSon); }
    if (node->rightSon != nullptr) { q.push(node->rightSon); }
  }
}

/**
 * Imprime a árvore de maneira formatada.
 * Exibe o nó raiz e seus filhos (esquerdo e direito) recursivamente.
 * Se um nó não tiver filhos, não será impresso nada para aquele lado.
 * @param root Ponteiro para a raiz da árvore.
 */
void printTree(TreeNode* root) {
  if (root == nullptr) {
    std::cout << "Árvore vazia";
    return;
  }

  cout << "(" << root->value;

  if (root->leftSon != nullptr || root->rightSon != nullptr) {
    cout << " ";

    if (root->leftSon != nullptr) {
      cout << "[L: ";
      printTree(root->leftSon);
      cout << "]";
    }

    if (root->rightSon != nullptr) {
      cout << " [R: ";
      printTree(root->rightSon);
      cout << "]";
    }
  }
  cout << ")";
}
