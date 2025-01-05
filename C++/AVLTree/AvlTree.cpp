#include <iostream>
#include <queue>
#include <iomanip>

using namespace std;

struct Node {
    int key;
    int height;
    Node* left;
    Node* right;

    Node(int val) : key(val), height(1), left(nullptr), right(nullptr) {}
};

/**
 * Função para obter a altura de um nó
 * @param node Node* nó
 * @return int altura
 */
int height(Node* node) {
    return node ? node->height : 0;
}

/**
 * Função para obter o fator de balanceamento de um nó
 * @param node Node* nó
 * @return int fator de balanceamento
 */
int getBalance(Node* node) {
    return node ? height(node->left) - height(node->right) : 0;
}

/**
 * Função para atualizar a altura de um nó
 * @param node Node* nó
 * @return void
 */
void updateHeight(Node* node) {
    if (node)
        node->height = 1 + max(height(node->left), height(node->right));
}

/**
 * Função para rotacionar um nó para a direita
 * @param y Node* nó
 * @return Node* nó
 */
Node* rightRotate(Node* y) {
    Node* x = y->left;
    Node* T2 = x->right;

    x->right = y;
    y->left = T2;

    updateHeight(y);
    updateHeight(x);

    return x;
}

/**
 * Função para rotacionar um nó para a esquerda
 * @param x Node* nó
 * @return Node* nó
 */
Node* leftRotate(Node* x) {
    Node* y = x->right;
    Node* T2 = y->left;

    y->left = x;
    x->right = T2;

    updateHeight(x);
    updateHeight(y);

    return y;
}

/**
 * Função para balancear um nó
 * @param node Node* nó
 * @return Node* nó balanceado
 */
Node* balance(Node* node) {
    int balanceFactor = getBalance(node);

    if (balanceFactor > 1) {
        if (getBalance(node->left) < 0)
            node->left = leftRotate(node->left);
        return rightRotate(node);
    }
    if (balanceFactor < -1) {
        if (getBalance(node->right) > 0)
            node->right = rightRotate(node->right);
        return leftRotate(node);
    }

    return node;
}

/**
 * Função para inserir um nó em uma árvore AVL
 * @param root Node* raiz
 * @param key int valor
 * @return Node* raiz
 */
Node* insert(Node* root, int key) {
    if (!root) return new Node(key);

    if (key < root->key)
        root->left = insert(root->left, key);
    else if (key > root->key)
        root->right = insert(root->right, key);
    else
        return root; // Duplicados não são permitidos

    updateHeight(root);
    return balance(root);
}

/**
 * Função para buscar um nó em uma árvore AVL
 * @param root Node* raiz
 * @param key int valor
 * @return bool verdadeiro se o valor for encontrado, caso contrário falso
 */
bool search(Node* root, int key) {
    if (!root) return false;
    if (root->key == key) return true;
    return key < root->key ? search(root->left, key) : search(root->right, key);
}

/**
 * Função para obter o nó com o menor valor em uma árvore
 * @param node Node* nó raiz
 * @return Node* nó com o menor valor
 */
Node* minValueNode(Node* node) {
    Node* current = node;
    while (current->left)
        current = current->left;
    return current;
}

/**
 * Função para obter o nó com o maior valor em uma árvore
 * @param node Node* nó raiz
 * @return Node* nó com o maior valor
 */
Node* maxValueNode(Node* node) {
    Node* current = node;
    while (current->right)
        current = current->right;
    return current;
}

/**
 * Função para remover um nó de uma árvore AVL
 * @param root Node* raiz
 * @param key int valor
 * @return Node* raiz
 */
Node* remove(Node* root, int key) {
    if (!root) return root;

    if (key < root->key)
        root->left = remove(root->left, key);
    else if (key > root->key)
        root->right = remove(root->right, key);
    else {
        if (!root->left || !root->right) {
            Node* temp = root->left ? root->left : root->right;
            delete root;
            return temp;
        }
        Node* temp = minValueNode(root->right);
        root->key = temp->key;
        root->right = remove(root->right, temp->key);
    }

    updateHeight(root);
    return balance(root);
}

/**
 * Função para imprimir os valores de uma árvore AVL em nível
 * @param root Node* raiz
 * @return void
 */
//TODO: Melhorar a impressão da árvore
void printLevelOrder(Node* root) {
    if (!root) return;

    queue<Node*> q;
    q.push(root);

    while (!q.empty()) {
        int size = q.size();
        while (size--) {
            Node* node = q.front();
            q.pop();
            cout << node->key << " ";
            if (node->left) q.push(node->left);
            if (node->right) q.push(node->right);
        }
        cout << endl;
    }
}

/**
 * Função principal para testar a árvore AVL implementada
 */
int main() {
    Node* root = nullptr;
    int values[] = {15, 18, 20, 35, 32, 38, 30, 40, 32, 45, 48, 52, 60, 50};

    for (int value : values)
        root = insert(root, value);

    cout << "Árvore AVL em nível:\n";
    printLevelOrder(root);

    return 0;
}
