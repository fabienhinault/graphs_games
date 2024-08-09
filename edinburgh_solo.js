"use strict";

import {firstPlayer, secondPlayer, Game, Evaluator, LocalStorageSequenceValueStorage, Clock} from './model.js';
import { voronoize } from './voronoize.js';

function moveEdgesLast() {
    document.querySelectorAll('#graph0 > g.edge').forEach(g => {
        g.parentElement.appendChild(g);
    });
}

function getNodeId(svgNode) {
    // remove "id"
    return Number(svgNode.id.substring(2));
}

function updateSvgCurrentVertex(currentSvgNode) {
    let ellipse = currentSvgNode.querySelector('ellipse');
    ellipse.setAttribute('fill', 'gray');
}

function updateSvgPreviousVertex(previousSvgNode, game) {
    document.querySelectorAll(`g._${getNodeId(previousSvgNode)}_`).forEach(g => {
        if (g.getAttribute('class').includes(`_${game.getCurrentMove()}_`)) {
            g.querySelector('path').setAttribute('stroke', 'lightgray');
        } else if (game.moves.length < 3 || !g.getAttribute('class').includes(`_${game.moves[game.moves.length - 3]}_`)) {
            g.remove();
        }
    });
    const lastCircle = previousSvgNode.querySelector('ellipse');
    lastCircle.setAttribute('stroke', 'lightgray');
    lastCircle.setAttribute('fill', 'none');
    if (game.moves.length === 2) {
        lastCircle.setAttribute('stroke-width', '3');
    }
    previousSvgNode.querySelector('text').setAttribute('style', 'fill: lightgray;');
}


document.addEventListener('DOMContentLoaded', async function() {
    const path = new URL(window.location.toLocaleString()).searchParams.get('path');
    const {nextss} = await import(`${path}.js`);
    const svgContent = await fetch(`${path}.svg`);
    const svgHtml = await svgContent.text();
    let game;
    let current;
    let previous;
    let evaluator;
    init();

    function init() {
        const tmpSvg = document.querySelector('body > svg');
        tmpSvg.innerHTML = svgHtml;
        tmpSvg.replaceWith(...tmpSvg.childNodes);
        game = new Game(nextss, JSON.parse(JSON.stringify(nextss)), new Clock(), null);
        current = undefined;
        previous = undefined;
        evaluator = new Evaluator(game, onGameOver, new LocalStorageSequenceValueStorage(), null);
        game.gameOverCallback = evaluator.onGameOver.bind(evaluator);
        const graphElement = document.querySelector('#graph0');
        const parentSvg = graphElement.closest('svg');
        moveEdgesLast();
        voronoize(parentSvg, graphElement);
        document.body.removeEventListener('click', init);
        document.body.addEventListener('click', onFirstClick);
        document.querySelectorAll('body > img').forEach(img => img.setAttribute('class', 'playing'));
    }

    function onGameOver(winnerName) {
        const klass = `${winnerName}_won`;
        ['#robot_won', '#player_won']
            .map(idSelector => document.querySelector(idSelector))
            .forEach(img => img.setAttribute('class', klass));
        document.body.removeEventListener('click', onClick);
        document.body.addEventListener('click', init);
    }

    function someonePlays(svgNode) {
        previous = current;
        current = svgNode;
        const idNumber = getNodeId(current);
        game.play(idNumber);
        evaluator.pushValue();
        updateSvgCurrentVertex(current);
        if (previous !== undefined) {
            updateSvgPreviousVertex(previous, game);
        }
    }

    function robotPlays() {
        const botNodeId = evaluator.chooseNext();
        const botSvgNode = document.querySelector(`g#id${botNodeId}`);
        someonePlays(botSvgNode);
        document.body.addEventListener('click', onClick);
        console.debug(evaluator.getSequenceValue(game.moves));
        console.debug(localStorage.length);
    }

    function robotThinksAndPlays() {
        if (game.possibleNexts.length > 0) {
            evaluator.evaluateNexts(game.clock.getTime() + 900);
            setTimeout(robotPlays, 1000);
        }
    }

    function onClick(event) {
        const tmp = event.target.closest('g#graph0 > g.node');
        if (tmp && (game.possibleNexts === undefined || game.possibleNexts.includes(getNodeId(tmp)))) {
            document.body.removeEventListener('click', onClick);
            playRound(tmp);
        }
    }

    function playRound(svgNode) {
        someonePlays(svgNode);
        robotThinksAndPlays();
    }

    function onFirstClick(event) {
        const svgNode = event.target.closest('g#graph0 > g.node');
        if (svgNode) {
            document.body.removeEventListener('click', onFirstClick);
            removeRobotSvgNode();
            if (svgNode.id === 'robot_begins') {
                robotBegins();
            } else {
                playerBegins(svgNode);
            }
        }
    }

    function removeRobotSvgNode() {
        document.querySelector('g.node#robot_begins').remove();
    }

    function playerBegins(svgNode) {
        firstPlayer.name = 'player';
        secondPlayer.name = 'robot';
        evaluator.player = secondPlayer;
        playRound(svgNode);
    }

    function robotBegins() {
        firstPlayer.name = 'robot';
        secondPlayer.name = 'player';
        evaluator.player = firstPlayer;
        robotThinksAndPlays();
    }
});


