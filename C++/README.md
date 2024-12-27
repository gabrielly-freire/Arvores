# EDBII - 2ª Unidade

## Como rodar e compilar

Inicialmente, tenha o repositório clonado. Com isso, vá para pasta raiz, que é a
`/C++`. Agora:
- **Heap**

Abra o terminal que desejar e execute o seguinte comando:

```
g++ TestHeap.cpp Heap/Heap.cpp -o  testHeap
```

Agora, basta executar o comando `./testHeap`. Se obter sucesso, terá o seguinte no terminal:

```
>>
```

Execute o comando `--help` para verificar as funções de teste na heap.

**OBS**: _Se deseja executar a função que computa o tempo de processamento na ordenação de 
listas com 10k, 100k e 1M de elemetos, é inteligente verificar se os arquivos `.txt` possuem 
dados. Eles se encontram no diretório `/Heap`. Se estiverem vazios ou se não existerem arquivos, basta executar os seguintes passos:_

```
g++ Heap/Util.cpp -o Heap/Util
```
_Após isso, execute:_

```
./Heap/Util
```
_Verifique se foram criados novos dados._

- **Árvore de Busca Binária**;

Abra o terminal que desejar e execute o seguinte comando:
````
g++ Tree.cpp TestBinTree.cpp SearchBinTree/SearchBinTree.cpp -o testBinTree
````
Por fim, execute o arquivo compilado com `./testBinTree`. Se tudo ocorrer com sucesso, vocẽ terá o seguinte no terminal:
```
>>
```
Execute `--help` para ver as funções que você pode executar para testar a definição de árvore de busca binária.
