"use strict";

function createSvgPoint(svg, vertex) {
    let svgPoint = svg.createSVGPoint();
    svgPoint.x = vertex.x;
    svgPoint.y = vertex.y;
    return svgPoint;
}

function createPolygon(svg, svgVertex, cell) {
    const hes = cell.halfedges;
    const firstHalfEdge = hes[0];
    let polygon = document.createElementNS("http://www.w3.org/2000/svg", "polygon");
    polygon.points.appendItem(createSvgPoint(svg, firstHalfEdge.getStartpoint()));
    for (const halfEdge of hes) {
        polygon.points.appendItem(createSvgPoint(svg, halfEdge.getEndpoint()));
    }
    if (!cell.closeMe) {
        polygon.points.appendItem(createSvgPoint(svg, firstHalfEdge.getStartpoint()));
    }
    polygon.setAttribute('fill', 'white');
    svgVertex.insertBefore(polygon, svgVertex.firstChild); 
}

function moveEdgesLast() {
    document.querySelectorAll('#graph0 > g.edge').forEach(g => {
        g.parentElement.appendChild(g);
    });
}

function voronoize() {
    const box = document.querySelector('#graph0 > polygon');
    const points = box.points;
    let voronoi = new Voronoi();
    const bbox = {xl: points[0].x, xr: points[2].x, yt: points[1].y, yb: points[0].y};
    const vertices = [...document.querySelectorAll('#graph0 > g.node')];
    let sites = vertices.map(g => g.querySelector('ellipse')).map(e => {return {x: Number(e.getAttribute('cx')), y: Number(e.getAttribute('cy'))};});
    const diagram = voronoi.compute(sites, bbox);
    const svg = box.closest('svg');
    for (let iVertex = 0; iVertex < vertices.length; iVertex++) {
        const voronoiId = sites[iVertex].voronoiId;
        const cell = diagram.cells[voronoiId];
        createPolygon(svg, vertices[iVertex], cell);
    }
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
    moveEdgesLast();
    voronoize();
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
        let ellipse = current.querySelector('ellipse');
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
        const previousCircle = previous.querySelector('ellipse');
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
    
