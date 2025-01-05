#include <iostream>
#include <queue>
#include <fstream>

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
 * Função para rotacionar um nó duplamente para a direita
 * @param x Node* nó
 * @return Node* nó
 */
Node* duplicateRightRotate(Node* y) {
    y->left = leftRotate(y->left);
    return rightRotate(y);
}

/**
 * Função para rotacionar um nó duplamente para a esquerda
 * @param x Node* nó
 * @return Node* nó
 */
Node* duplicateLeftRotate(Node* x) {
    x->right = rightRotate(x->right);
    return leftRotate(x);
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
        return root;

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
 * Função para gerar a representação em formato DOT de um nó e seus filhos em uma árvore AVL.
 * @param root Node* raiz
 * @param file ofstream arquivo de saída
 */
void generateDOT(Node* root, ofstream& file) {
    if (!root) return;

    if (root->left) {
        file << "    " << root->key << " -> " << root->left->key << ";\n";
        generateDOT(root->left, file);
    }

    if (root->right) {
        file << "    " << root->key << " -> " << root->right->key << ";\n";
        generateDOT(root->right, file);
    }
}

/**
 * Função para exportar uma árvore AVL para um arquivo JPEG
 * @param root Node* raiz
 * @param filename string nome do arquivo
 */
void exportToJPEG(Node* root, const string& filename) {
    string dotFile = filename + ".dot";
    string jpegFile = filename + ".jpeg";

    ofstream file(dotFile);
    file << "digraph AVLTree {\n";
    file << "    node [shape=circle];\n";

    if (root) {
        generateDOT(root, file);
    }

    file << "}\n";
    file.close();

    string command = "dot -Tjpeg " + dotFile + " -o " + jpegFile;
    int result = system(command.c_str());

    if (result == 0) {
        cout << "Arquivo JPEG gerado em: " << jpegFile << endl;
    } else {
        cerr << "Erro ao gerar o arquivo JPEG. Verifique se o Graphviz está instalado e acessível." << endl;
    }
}

/**
 * Função para exportar uma árvore AVL em nível conforme os valores dados pela questão
 */
void exportTreeInLevel() {
    Node* root = nullptr;
    int values[] = {15, 18, 20, 35, 32, 38, 30, 40, 32, 45, 48, 52, 60, 50};

    for (int value : values)
        root = insert(root, value);

     exportToJPEG(root, "avl_tree");
}
