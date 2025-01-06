#include <iostream>
#include <queue>

using namespace std;

struct NoRB {
    int item;
    string cor;
    NoRB *esquerda, *direita, *pai;

    NoRB(int chave) : item(chave), cor("Vermelho"), esquerda(nullptr), direita(nullptr), pai(nullptr) {}
};

class ArvoreRubroNegra {
private:
    NoRB* TNULL;
    NoRB* root;

    void inicializarTNULL() {
        TNULL = new NoRB(0);
        TNULL->cor = "Preto";
        TNULL->esquerda = nullptr;
        TNULL->direita = nullptr;
    }

    void rotacaoEsquerda(NoRB* x) {
        NoRB* y = x->direita;
        x->direita = y->esquerda;
        if (y->esquerda != TNULL) {
            y->esquerda->pai = x;
        }
        y->pai = x->pai;
        if (x->pai == nullptr) {
            root = y;
        } else if (x == x->pai->esquerda) {
            x->pai->esquerda = y;
        } else {
            x->pai->direita = y;
        }
        y->esquerda = x;
        x->pai = y;
    }

    void rotacaoDireita(NoRB* x) {
        NoRB* y = x->esquerda;
        x->esquerda = y->direita;
        if (y->direita != TNULL) {
            y->direita->pai = x;
        }
        y->pai = x->pai;
        if (x->pai == nullptr) {
            root = y;
        } else if (x == x->pai->direita) {
            x->pai->direita = y;
        } else {
            x->pai->esquerda = y;
        }
        y->direita = x;
        x->pai = y;
    }

    void corrigirInsercao(NoRB* k) {
        while (k->pai && k->pai->cor == "Vermelho") {
            if (k->pai == k->pai->pai->esquerda) {
                NoRB* tio = k->pai->pai->direita;
                if (tio && tio->cor == "Vermelho") {
                    k->pai->cor = "Preto";
                    tio->cor = "Preto";
                    k->pai->pai->cor = "Vermelho";
                    k = k->pai->pai;
                } else {
                    if (k == k->pai->direita) {
                        k = k->pai;
                        rotacaoEsquerda(k);
                    }
                    k->pai->cor = "Preto";
                    k->pai->pai->cor = "Vermelho";
                    rotacaoDireita(k->pai->pai);
                }
            } else {
                NoRB* tio = k->pai->pai->esquerda;
                if (tio && tio->cor == "Vermelho") {
                    k->pai->cor = "Preto";
                    tio->cor = "Preto";
                    k->pai->pai->cor = "Vermelho";
                    k = k->pai->pai;
                } else {
                    if (k == k->pai->esquerda) {
                        k = k->pai;
                        rotacaoDireita(k);
                    }
                    k->pai->cor = "Preto";
                    k->pai->pai->cor = "Vermelho";
                    rotacaoEsquerda(k->pai->pai);
                }
            }
        }
        root->cor = "Preto";
    }

    void corrigirRemocao(NoRB* x) {
        while (x != root && x->cor == "Preto") {
            if (x == x->pai->esquerda) {
                NoRB* s = x->pai->direita;
                if (s->cor == "Vermelho") {
                    s->cor = "Preto";
                    x->pai->cor = "Vermelho";
                    rotacaoEsquerda(x->pai);
                    s = x->pai->direita;
                }
                if (s->esquerda->cor == "Preto" && s->direita->cor == "Preto") {
                    s->cor = "Vermelho";
                    x = x->pai;
                } else {
                    if (s->direita->cor == "Preto") {
                        s->esquerda->cor = "Preto";
                        s->cor = "Vermelho";
                        rotacaoDireita(s);
                        s = x->pai->direita;
                    }
                    s->cor = x->pai->cor;
                    x->pai->cor = "Preto";
                    s->direita->cor = "Preto";
                    rotacaoEsquerda(x->pai);
                    x = root;
                }
            } else {
                NoRB* s = x->pai->esquerda;
                if (s->cor == "Vermelho") {
                    s->cor = "Preto";
                    x->pai->cor = "Vermelho";
                    rotacaoDireita(x->pai);
                    s = x->pai->esquerda;
                }
                if (s->direita->cor == "Preto" && s->esquerda->cor == "Preto") {
                    s->cor = "Vermelho";
                    x = x->pai;
                } else {
                    if (s->esquerda->cor == "Preto") {
                        s->direita->cor = "Preto";
                        s->cor = "Vermelho";
                        rotacaoEsquerda(s);
                        s = x->pai->esquerda;
                    }
                    s->cor = x->pai->cor;
                    x->pai->cor = "Preto";
                    s->esquerda->cor = "Preto";
                    rotacaoDireita(x->pai);
                    x = root;
                }
            }
        }
        x->cor = "Preto";
    }

    void substituirNo(NoRB* u, NoRB* v) {
        if (u->pai == nullptr) {
            root = v;
        } else if (u == u->pai->esquerda) {
            u->pai->esquerda = v;
        } else {
            u->pai->direita = v;
        }
        v->pai = u->pai;
    }

public:
    ArvoreRubroNegra() {
        inicializarTNULL();
        root = TNULL;
    }

    void inserir(int chave) {
        NoRB* novoNo = new NoRB(chave);
        novoNo->esquerda = TNULL;
        novoNo->direita = TNULL;

        NoRB* y = nullptr;
        NoRB* x = root;

        while (x != TNULL) {
            y = x;
            if (novoNo->item < x->item) {
                x = x->esquerda;
            } else {
                x = x->direita;
            }
        }

        novoNo->pai = y;
        if (y == nullptr) {
            root = novoNo;
        } else if (novoNo->item < y->item) {
            y->esquerda = novoNo;
        } else {
            y->direita = novoNo;
        }

        if (novoNo->pai == nullptr) {
            novoNo->cor = "Preto";
            return;
        }

        if (novoNo->pai->pai == nullptr) {
            return;
        }

        corrigirInsercao(novoNo);
    }

    void remover(int chave) {
        NoRB* z = buscar(chave);
        if (z == TNULL) return;

        NoRB* y = z;
        NoRB* x;
        string yCorOriginal = y->cor;

        if (z->esquerda == TNULL) {
            x = z->direita;
            substituirNo(z, z->direita);
        } else if (z->direita == TNULL) {
            x = z->esquerda;
            substituirNo(z, z->esquerda);
        } else {
            y = minimo(z->direita);
            yCorOriginal = y->cor;
            x = y->direita;
            if (y->pai == z) {
                x->pai = y;
            } else {
                substituirNo(y, y->direita);
                y->direita = z->direita;
                y->direita->pai = y;
            }
            substituirNo(z, y);
            y->esquerda = z->esquerda;
            y->esquerda->pai = y;
            y->cor = z->cor;
        }

        if (yCorOriginal == "Preto") {
            corrigirRemocao(x);
        }
    }

    NoRB* buscar(int chave) {
        NoRB* atual = root;
        while (atual != TNULL && chave != atual->item) {
            if (chave < atual->item) {
                atual = atual->esquerda;
            } else {
                atual = atual->direita;
            }
        }
        return atual;
    }

    NoRB* minimo(NoRB* no) {
        while (no->esquerda != TNULL) {
            no = no->esquerda;
        }
        return no;
    }

    void emNivel() {
        if (root == TNULL) {
            cout << "\u00c1rvore está vazia." << endl;
            return;
        }

        queue<NoRB*> fila;
        fila.push(root);

        while (!fila.empty()) {
            NoRB* atual = fila.front();
            fila.pop();
            cout << atual->item << " (" << atual->cor << ") ";

            if (atual->esquerda != TNULL) {
                fila.push(atual->esquerda);
            }

            if (atual->direita != TNULL) {
                fila.push(atual->direita);
            }
        }
        cout << endl;
    }
};

int main() {
    ArvoreRubroNegra arvore;
    int opcao = 0;

    while (opcao != 5) {
        cout << "*************************************" << endl;
        cout << "Entre com a opcao:" << endl;
        cout << "** 1 **: Inserir valor" << endl;
        cout << "** 2 **: Remover valor" << endl;
        cout << "** 3 **: Buscar valor" << endl;
        cout << "** 4 **: Exibir Arvore" << endl;
        cout << "** 5 **: Sair do programa" << endl;
        cout << "*************************************" << endl;
        cout << "-> ";
        cin >> opcao;

        if (opcao == 1) {
            int x;
            cout << "Informe o valor -> ";
            cin >> x;
            arvore.inserir(x);
        } else if (opcao == 2) {
            int x;
            cout << "Informe o valor -> ";
            cin >> x;
            arvore.remover(x);
        } else if (opcao == 3) {
            int x;
            cout << "Informe o valor -> ";
            cin >> x;
            NoRB* resultado = arvore.buscar(x);
            if (resultado != nullptr) {
                cout << "Valor " << x << " encontrado na arvore." << endl;
            } else {
                cout << "Valor " << x << " nao encontrado na arvore." << endl;
            }
        } else if (opcao == 4) {
            arvore.emNivel();
        } else if (opcao == 5) {
            cout << "Saindo do programa..." << endl;
            break;
        } else {
            cout << "Opcao invalida! Tente novamente." << endl;
        }
    }

    return 0;
}