
let previous;
let current;
let game;

function onGameOver(winner) {
    const klass = ['robot_won', 'player_won'][winner];
    ['#robot_won', '#player_won']
        .map(idSelector => document.querySelector(idSelector))
        .forEach(img => img.setAttribute('class', klass));
}

function getNodeId(node) {
    return node.id.substring(2);
}

function updateSvgCurrentVertex() {
    let ellipse = current.querySelector('ellipse');
    ellipse.setAttribute('fill', 'gray');
}

function updateSvgLastVertex(previous) {
    document.querySelectorAll(`g._${game.getPreviousMove()}_`).forEach(g => {
        if (g.getAttribute('class').includes(`_${game.getCurrentMove()}_`)) {
            g.querySelector('path').setAttribute('stroke', 'lightgray');
        } else if (game.moves.length < 3 || !g.getAttribute('class').includes(`_${game.moves[game.moves.length - 3]}_`)) {
            g.remove();
        }
    });
    const lastCircle = previous.querySelector('ellipse');
    lastCircle.setAttribute('stroke', 'lightgray');
    lastCircle.setAttribute('fill', 'none');
    if (game.moves.length === 2) {
        lastCircle.setAttribute('stroke-width', '3');
    }
    previous.querySelector('text').setAttribute('style', 'fill: lightgray;');
}


