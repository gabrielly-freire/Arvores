#include <stdio.h>
#include <stdlib.h>

// Definindo a estrutura nodo
struct nodo {
    int chave;        // Chave do nó
    int conteudo;     // Conteúdo do nó
    struct nodo *pfilho;  // Ponteiro para o primeiro filho
    struct nodo *irmao;   // Ponteiro para o irmão
};

// Definindo o tipo no para representar a estrutura nodo
typedef struct nodo no;

// Função para criar um novo nó
no* cria_no(int chave, int conteudo) {
    no *novo_no = (no *)malloc(sizeof(no));
    novo_no->chave = chave;
    novo_no->conteudo = conteudo;
    novo_no->pfilho = NULL;
    novo_no->irmao = NULL;
    return novo_no;
}

// Função para adicionar um filho a um nó
void adiciona_filho(no *pai, no *filho) {
    if (pai->pfilho == NULL) {
        pai->pfilho = filho;  // Se o pai não tem filhos, o filho se torna o primeiro
    } else {
        no *temp = pai->pfilho;
        while (temp->irmao != NULL) {
            temp = temp->irmao;  // Vai até o último irmão
        }
        temp->irmao = filho;  // Adiciona o novo filho como irmão do último filho
    }
}

// Função para imprimir a árvore
void imprime_arvore(no *n, int nivel) {
    if (n == NULL) return;
    
    // Imprime o nó atual com a indentação de acordo com o nível
    for (int i = 0; i < nivel; i++) printf("  ");
    printf("Chave: %d, Conteúdo: %d\n", n->chave, n->conteudo);
    
    // Chama recursivamente para imprimir os filhos
    imprime_arvore(n->pfilho, nivel + 1);
    
    // Chama recursivamente para imprimir os irmãos
    imprime_arvore(n->irmao, nivel);
}

int main() {
    // Criando a árvore
    no *raiz = cria_no(1, 10);  // Criando a raiz da árvore
    no *filho1 = cria_no(2, 20);
    no *filho2 = cria_no(3, 30);
    no *filho3 = cria_no(4, 40);
    
    // Adicionando filhos à raiz
    adiciona_filho(raiz, filho1);
    adiciona_filho(raiz, filho2);
    adiciona_filho(raiz, filho3);
    
    // Adicionando filhos ao filho1
    no *filho1_1 = cria_no(5, 50);
    no *filho1_2 = cria_no(6, 60);
    adiciona_filho(filho1, filho1_1);
    adiciona_filho(filho1, filho1_2);
    
    // Imprimindo a árvore
    printf("Árvore:\n");
    imprime_arvore(raiz, 0);
    
    // Liberando memória
    free(raiz);
    free(filho1);
    free(filho2);
    free(filho3);
    free(filho1_1);
    free(filho1_2);

    return 0;
}

