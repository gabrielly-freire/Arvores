import BinarySearchTree from '../binary-tree/BinarySearchTree.js';

export default class AVLTree extends BinarySearchTree {
    constructor(compareFn = defaultCompare) {
        super(compareFn);
        this.compareFn = compareFn;
        this.root = null;
    }

    BalanceFactor = {
        UNBALANCED_RIGHT: 1,
        SLIGHTLY_UNBALANCED_RIGHT: 2,
        BALANCED: 3,
        SLIGHTLY_UNBALANCED_LEFT: 4,
        UNBALANCED_LEFT: 5
    };

    /**
     * Rotação simples à direita
     * @param {*} node 
     * @returns 
     */
    rotateLL(node) {
        const tmp = node.left;
        node.left = tmp.right;
        tmp.right = node;
        return tmp;
    }

    /**
     * Rotação simples à esquerda
     * @param {*} node 
     * @returns 
     */
    rotateRR(node) {
        const tmp = node.right;
        node.right = tmp.left;
        tmp.left = node;
        return tmp;
    }

    /**
     * Rotação dupla à direita
     * @param {*} node 
     * @returns 
     */
    rotateLR(node) {
        node.left = this.rotateRR(node.left);
        return this.rotateLL(node);
    }

    /**
     * Rotação dupla à esquerda
     * @param {*} node 
     * @returns 
     */
    rotateRL(node) {
        node.right = this.rotateLL(node.right);
        return this.rotateRR(node);
    }

    /**
     * Retorna o fator de balanceamento de um nó
     * @param {*} node
     * @returns 
     */
    getBalanceFactor(node) {
        const heightDifference = this.getNodeHeight(node.left) - this.getNodeHeight(node.right);

        switch (heightDifference) {
            case -2:
                return this.BalanceFactor.UNBALANCED_RIGHT;
            case -1:
                return this.BalanceFactor.SLIGHTLY_UNBALANCED_RIGHT;
            case 1:
                return this.BalanceFactor.SLIGHTLY_UNBALANCED_LEFT;
            case 2:
                return this.BalanceFactor.UNBALANCED_LEFT;
            default:
                return this.BalanceFactor.BALANCED;
        }
    }

    /**
     * Insere um novo elemento na árvore AVL
     * @param {*} key 
     */
    insert(key) {
        this.root = this.insertNode(this.root, key);
    }

    insertNode(node, key) {
        if (node == null) {
            return new Node(key);
        } else if (this.compareFn(key, node.key) === Compare.LESS_THAN) {
            node.left = this.insertNode(node.left, key);
        } else if (this.compareFn(key, node.key) === Compare.BIGGER_THAN) {
            node.right = this.insertNode(node.right, key);
        } else {
            return node;
        }

        const balanceState = this.getBalanceFactor(node);
        if (balanceState === BalanceFactor.UNBALANCED_LEFT) {
            if (this.compareFn(key, node.left.key) === Compare.LESS_THAN) {
                node = this.rotateLL(node);
            } else {
                return this.rotateLR(node);
            }
        }

        if (balanceState === BalanceFactor.UNBALANCED_RIGHT) {
            if (this.compareFn(key, node.right.key) === Compare.BIGGER_THAN) {
                node = this.rotateRR(node);
            } else {
                return this.rotateRL(node);
            }
        }

        return node;
    }

    /**
     * Remove um elemento da árvore AVL
     * @param {*} key 
     */
    remove(key) {
        this.root = this.removeNode(this.root, key);
    }

    removeNode(node, key) {
        node = super.removeNode(node, key);

        if (node == null) {
            return node;
        }

        const balanceState = this.getBalanceFactor(node);
        if (balanceState === BalanceFactor.UNBALANCED_LEFT) {
            const balanceStateLeft = this.getBalanceFactor(node.left);
            if (balanceStateLeft === BalanceFactor.BALANCED || balanceStateLeft === BalanceFactor.SLIGHTLY_UNBALANCED) {
                return this.rotateLL(node);
            }

            if (balanceStateLeft === BalanceFactor.UNBALANCED_RIGHT) {
                return this.rotateLR(node);
            }
        }

        if (balanceState === BalanceFactor.UNBALANCED_RIGHT) {
            const balanceStateRight = this.getBalanceFactor(node.right);
            if (balanceStateRight === BalanceFactor.BALANCED || balanceStateRight === BalanceFactor.SLIGHTLY_UNBALANCED) {
                return this.rotateRR(node);
            }

            if (balanceStateRight === BalanceFactor.UNBALANCED_LEFT) {
                return this.rotateRL(node.right);
            }
        }

        return node;
    }

    search(key) {
        return this.searchNode(this.root, key);
    }

    searchNode(node, key) {
        if (node == null) {
            return false;
        }

        if (this.compareFn(key, node.key) === Compare.LESS_THAN) {
            return this.searchNode(node.left, key);
        } else if (this.compareFn(key, node.key) === Compare.BIGGER_THAN) {
            return this.searchNode(node.right, key);
        } else {
            return true;
        }
    }

}
