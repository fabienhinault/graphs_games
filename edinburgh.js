"use strict";
function enlargeVertices() {
    document.querySelectorAll('g.node').forEach(g => {
        // all vertices seem to be at least 18 away from each other.
        const extraRadius = 9;
        const ellipse = g.querySelector('ellipse');
        const clonedEllipse = ellipse.cloneNode(false);
        clonedEllipse.removeAttribute('stroke');
        clonedEllipse.setAttribute('rx', Number(clonedEllipse.getAttribute('rx')) + extraRadius);
        clonedEllipse.setAttribute('ry', Number(clonedEllipse.getAttribute('ry')) + extraRadius);
        ellipse.insertAdjacentElement('beforebegin', clonedEllipse);
    });
}

class Clock {
    getTime() {
        return Date.now();
    }
}

class Game {
    constructor(nextss, initialNextss, sequenceValueStorage, clock, gameOverCallback) {
        this.nextss = nextss;
        this.moves = [];
        this.possibleNexts = undefined;
        this.initialNextss = initialNextss;
        this.sequenceValueStorage= sequenceValueStorage;
        this.clock = clock;
        this.gameOverCallback = gameOverCallback;
    }
    
    copy() {
        const result = new Game([...this.nextss.map(_ => [..._])], this.initialNextss, this.sequenceValueStorage, this.clock, null);
        result.moves = [...this.moves];
        return result;
    }

    getCurrentMove() {
        return this.moves[this.moves.length - 1];
    }

    getPreviousMove() {
        return this.moves[this.moves.length - 2];
    }

    getCurrentPlayer() {
        return (this.moves.length % 2) * firstPlayer;
    }

    minmax(nextPlayer, nextsValues) {
        if (nextPlayer === firstPlayer) {
            return Math.max(...nextsValues);
        } else {
            return Math.min(...nextsValues);
        }
    }

    getSequenceValue(sequence) {
        const storedValue = this.sequenceValueStorage.getValue(sequence);
        if (storedValue !== null && storedValue !== undefined) {
            return Number(storedValue);
        }
        return unsure;
    }

    evaluateNexts(time) {
        if (this.clock.getTime() < time) {
            for (let next of this.possibleNexts) {
                const game = this.copy();
                game.play(next);
                setTimeout(() => game.evaluateNexts(time), 0);
            }
        }
    }

    play(current) {
        this.moves.push(current);
        this.possibleNexts = [...this.nextss[current]];
        this.nextss = this.nextss.map((nexts, iNexts) => {
            return nexts.filter(next => next != current);
        });
        this.nextss[current] = [];
        if (this.possibleNexts.length === 0) {
            // the game is over
            this.gameOverCallback?.();
        }
    }

    getMoveValue(nextId) {
        return this.getSequenceValue([...this.moves, nextId]);
    }
}

class LocalStorageSequenceValueStorage {
    storeValue(sequence, value) {
        localStorage.setItem(sequence, value);
    }
    getValue(sequence) {
        return localStorage.getItem(sequence);
    }
    removeValue(sequence) {
        localStorage.removeItem(sequence);
    }
}

document.addEventListener('DOMContentLoaded', function() {
    function onGameOver() {
        setTimeout(() => {
            const klass = ['player2_won', 'player1_won'][game.moves.length % 2];
            ['#player1_won', '#player2_won']
                .map(idSelector => document.querySelector(idSelector))
                .forEach(img => img.setAttribute('class', klass));
        }, 1000);
    }

    const game = new Game(nextss, JSON.parse(JSON.stringify(nextss)), new LocalStorageSequenceValueStorage(), new Clock(), onGameOver);
    enlargeVertices();
    let prepreviousId;
    let previous;
    let current;
    let possibleIds;

    function getNodeId(node) {
        // pass prefix "id"
        return node.id.substring(2);
    }

    function play(tmp) {
        if (previous !== undefined) {
            prepreviousId = getNodeId(previous);
        }
        previous = current;
        current = tmp;
        game.play(getNodeId(current));
        updateSvgCurrentVertex();
        if (previous !== undefined) {
            updateSvgpreviousVertex(previous);
        }
    }

    function updateSvgCurrentVertex() {
        let ellipse = current.querySelector('ellipse + ellipse');
        ellipse.setAttribute('fill', 'gray');
    }

    function updateSvgpreviousVertex(previous) {
        const previousId = getNodeId(previous);
        const currentId = getNodeId(current);
        document.querySelectorAll(`g._${previousId}_`).forEach(g => {
            if (g.getAttribute('class').includes(`_${currentId}_`)) {
                g.querySelector('path').setAttribute('stroke', 'lightgray');
            } else if (prepreviousId === undefined || !g.getAttribute('class').includes(`_${prepreviousId}_`)) {
                g.remove();
            }
        });
        const previousCircle = previous.querySelector('ellipse + ellipse');
        previousCircle.setAttribute('stroke', 'lightgray');
        previousCircle.setAttribute('fill', 'none');
        if (prepreviousId === undefined) {
            previousCircle.setAttribute('stroke-width', '3');
        }
        previous.querySelector('text').setAttribute('style', 'fill: lightgray;');
    }



    document.body.onclick = (event) => {
        const tmp = event.target.closest('svg > g > g.node');
        if (tmp && (possibleIds === undefined || possibleIds.includes(getNodeId(tmp)))) {
            play(tmp);
            possibleIds = nextss[getNodeId(current)];
        }
    }
});
    
