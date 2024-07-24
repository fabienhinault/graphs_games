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

document.addEventListener('DOMContentLoaded', async function() {
    const path = new URL(window.location.toLocaleString()).searchParams.get('path');
    const {nextss} = await import(`${path}.js`);
    const svg = document.querySelector('#svg');
    svg.innerHTML = await (await fetch(`${path}.svg`)).text();
    svg.replaceWith(...svg.childNodes);
    game = new Game(nextss, JSON.parse(JSON.stringify(nextss)), new Clock(), null);
    const evaluator = new Evaluator(game, onGameOver, new LocalStorageSequenceValueStorage());
    game.gameOverCallback = evaluator.onGameOver.bind(evaluator);
    moveEdgesLast();
    voronoize();

    function play(tmp) {
        previous = current;
        current = tmp;
        const idNumber = getNodeId(current);
        game.play(idNumber);
        evaluator.pushValue();
        updateSvgCurrentVertex();
        if (previous !== undefined) {
            updateSvgLastVertex(previous);
        }
    }

    function robotPlays() {
        const botChoice = evaluator.chooseNext();
        const botElement = document.querySelector(`g#id${botChoice}`);
        play(botElement);
        console.debug(evaluator.getSequenceValue(game.moves));
        console.debug(localStorage.length);
    }

    document.body.onclick = (event) => {
        const tmp = event.target.closest('svg > g > g.node');
        if (tmp && (game.possibleNexts === undefined || game.possibleNexts.includes(getNodeId(tmp)))) {
            play(tmp);
            if (game.possibleNexts.length > 0) {
                evaluator.evaluateNexts(game.clock.getTime() + 900);
                setTimeout(robotPlays, 1000);
            }
        }
    }

    document.querySelector('#robot_begins').onclick = (event) => {
        robotPlays();
    }
});


