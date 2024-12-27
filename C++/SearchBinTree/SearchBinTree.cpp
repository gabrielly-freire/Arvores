//
// Criado por Vitor em 09/12/24.
//

#include <iostream>

#include "SearchBinTree.h"

using namespace std;

/**
 * Insere um novo nó com o valor fornecido na árvore binária de pesquisa.
 * Se a árvore estiver vazia, um novo nó é criado como raiz.
 * Caso contrário, encontra a posição apropriada para o novo nó.
 * @param root O nó raiz da árvore.
 * @param value O valor a ser inserido.
 * @return A raiz atualizada da árvore.
 */
TreeNode* insertNode(TreeNode* root, const int value) {
    if (root == nullptr) { return new TreeNode(value); }
    if (value < root->value) { root->leftSon = insertNode(root->leftSon, value); }
    else { root->rightSon = insertNode(root->rightSon, value); }
    return root;
}

/**
 * Pesquisa um nó com o valor fornecido na árvore binária de pesquisa.
 * Retorna verdadeiro se o valor for encontrado, caso contrário retorna falso.
 * @param root O nó raiz da árvore.
 * @param value O valor a ser pesquisado.
 * @return verdadeiro se o valor for encontrado, falso caso contrário.
 */
bool search(TreeNode* root, const int value) {
    if (root == nullptr) { return false; }
    if (value == root->value) { return true; }
    if (value < root->value) { return search(root->leftSon, value); }
    if (value > root->value) { return search(root->rightSon, value); }
    return false;
}

/**
 * Pesquisa um nó com o valor fornecido e seu pai na árvore binária de pesquisa.
 * @param root O nó raiz da árvore.
 * @param value O valor a ser pesquisado.
 * @param parent Um ponteiro para armazenar o pai do nó encontrado.
 * @return Um ponteiro para o nó se encontrado, nullptr se não encontrado.
 */
TreeNode* searchNode(TreeNode* root, int value, TreeNode** parent) {
    TreeNode* current = root;
    *parent = nullptr;

    while (current != nullptr) {
        if (current->value == value) { return current; }
        *parent = current;

        if (value < current->value) { current = current->leftSon; }
        else { current = current->rightSon; }
    }
    return nullptr;
}

/**
 * Encontra o nó com o valor mínimo na árvore binária de pesquisa.
 * O nó com o valor mínimo é o nó mais à esquerda da árvore.
 * @param root O nó raiz da árvore.
 * @return Um ponteiro para o nó com o valor mínimo.
 */
TreeNode* findMin(TreeNode* root) {
    while (root->leftSon != nullptr) { root = root->leftSon; }
    return root;
}

/**
 * Deleta um nó com o valor fornecido da árvore binária de pesquisa.
 * Trata três casos:
 * 1. O nó não tem filhos (nó folha).
 * 2. O nó tem um filho.
 * 3. O nó tem dois filhos (encontra o sucessor em ordem e o deleta).
 * @param root O nó raiz da árvore.
 * @param value O valor a ser deletado.
 * @return A raiz atualizada da árvore.
 */
TreeNode* deleteNode(TreeNode* root, const int value) {
    TreeNode* parent = nullptr;
    TreeNode* nodeToDelete = searchNode(root, value, &parent);

    if (nodeToDelete == nullptr) {
        cout << "Valor não encontrado!" << endl;
        return root;
    }

    // Primeiro caso: Não tem filhos (nó folha).
    if (nodeToDelete->leftSon == nullptr && nodeToDelete->rightSon == nullptr) {
        if (nodeToDelete == root) {
            delete root;
            return nullptr;
        }
        if (parent->leftSon == nodeToDelete) {
            parent->leftSon = nullptr;
        } else {
            parent->rightSon = nullptr;
        }
        delete nodeToDelete;
    }

    // Segundo caso: Um filho.
    else if (nodeToDelete->leftSon == nullptr || nodeToDelete->rightSon == nullptr) {
        TreeNode* child = (nodeToDelete->leftSon != nullptr) ? nodeToDelete->leftSon : nodeToDelete->rightSon;

        if (nodeToDelete == root) { root = child; }
        else if (parent->leftSon == nodeToDelete) { parent->leftSon = child; }
        else { parent->rightSon = child; }

        delete nodeToDelete;
    }

    // Terceiro caso: Dois filhos (encontra o sucessor em ordem e o deleta).
    else {
        TreeNode* successor = findMin(nodeToDelete->rightSon);
        int successorValue = successor->value;
        root = deleteNode(root, successorValue);
        nodeToDelete->value = successorValue;
    }

    return root;
}

/**
 * Verifica se a árvore binária é uma árvore binária de pesquisa válida usando a travessia em ordem.
 * Garante que o valor de cada nó seja maior do que o valor do nó anterior na travessia.
 * @param root O nó raiz da árvore.
 * @param prev Um ponteiro para armazenar o nó anterior durante a travessia.
 * @return verdadeiro se a árvore for uma árvore binária de pesquisa válida, falso caso contrário.
 */
bool isBSTInOrder(TreeNode* root, TreeNode*& prev) {
    if (root == nullptr) { return true; }
    if (!isBSTInOrder(root->leftSon, prev)) { return false; }
    if (prev != nullptr && root->value <= prev->value) { return false; }
    prev = root;
    return isBSTInOrder(root->rightSon, prev);
}

/**
 * Função de wrapper para verificar se a árvore binária é uma árvore binária de pesquisa válida.
 * Chama a função isBSTInOrder para validar a árvore.
 * @param root O nó raiz da árvore.
 * @return verdadeiro se a árvore for uma árvore binária de pesquisa válida, falso caso contrário.
 */
bool isBinarySearchTree(TreeNode* root) {
    TreeNode* prev = nullptr;
    return isBSTInOrder(root, prev);
}
