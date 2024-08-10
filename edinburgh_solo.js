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

class View {
    constructor(game) {
        this.game = game;
        this.onPlayedFn = this.onPlayed.bind(this);
        this.onGameOverFn = this.onGameOver.bind(this);
        this.game.addPlayedListener(this.onPlayedFn);
        this.game.addGameOverListener(this.onGameOverFn);
        this.currentSvgNode;
        this.previousSvgNode;
        document.querySelectorAll('body > img').forEach(img => img.setAttribute('class', 'playing'));
        const graphElement = document.querySelector('#graph0');
        const parentSvg = graphElement.closest('svg');
        moveEdgesLast();
        voronoize(parentSvg, graphElement);
    }

    stopListening() {
        this.game.dispatcher.removeEventListener("played", this.onPlayedFn);
        this.game.dispatcher.removeEventListener("game over", this.onGameOverFn);
    }

    onGameOver(evt) {
        const winnerName = evt.detail.winner;
        const klass = `${winnerName}_won`;
        ['#robot_won', '#player_won']
            .map(idSelector => document.querySelector(idSelector))
            .forEach(img => img.setAttribute('class', klass));
    }

    onPlayed(playedEvent) {
        const currentNodeId = playedEvent.detail.move;
        this.previousSvgNode = this.currentSvgNode;
        this.currentSvgNode = document.querySelector(`g#id${currentNodeId}`);
        this.updateSvgCurrentVertex();
        if (this.previousSvgNode !== undefined) {
            this.updateSvgPreviousVertex(currentNodeId);
        }
    }
   
    updateSvgCurrentVertex() {
        this.currentSvgNode.querySelector('ellipse').setAttribute('fill', 'gray');
    }

    updateSvgPreviousVertex(currentNodeId) {
        document.querySelectorAll(`g._${getNodeId(this.previousSvgNode)}_`).forEach(g => {
            if (g.getAttribute('class').includes(`_${currentNodeId}_`)) {
                g.querySelector('path').setAttribute('stroke', 'lightgray');
            } else if (this.game.moves.length < 3 || !g.getAttribute('class').includes(`_${this.game.getPrepreviousMove()}_`)) {
                // do not remove edge between previous and pre-previous
                g.remove();
            }
        });
        const lastCircle = this.previousSvgNode.querySelector('ellipse');
        lastCircle.setAttribute('stroke', 'lightgray');
        lastCircle.setAttribute('fill', 'none');
        if (this.game.moves.length === 2) {
            lastCircle.setAttribute('stroke-width', '3');
        }
        this.previousSvgNode.querySelector('text').setAttribute('style', 'fill: lightgray;');
    }

} 

document.addEventListener('DOMContentLoaded', async function() {
    const path = new URL(window.location.toLocaleString()).searchParams.get('path');
    const {nextss} = await import(`${path}.js`);
    const svgContent = await fetch(`${path}.svg`);
    const svgHtml = await svgContent.text();
    let game;
    let evaluator;
    let view;
    init();

    function init(evt) {
        evt?.stopPropagation();
        document.body.removeEventListener('click', init);
        const tmpSvg = document.querySelector('body > svg');
        tmpSvg.innerHTML = svgHtml;
        tmpSvg.replaceWith(...tmpSvg.childNodes);
        game = new Game(nextss, JSON.parse(JSON.stringify(nextss)), new Clock(), document);
        game.addGameOverListener(onGameOver);
        if (view) {
            // else, the old view continues listening
            view.stopListening();
        }
        view = new View(game);
        evaluator = new Evaluator(game, new LocalStorageSequenceValueStorage(), null);
        document.body.addEventListener('click', onFirstClick);
    }

    function onGameOver(evt) {
        document.body.removeEventListener('click', onClick);
        document.body.addEventListener('click', init);
    }

    function someonePlays(svgNode) {
        const idNumber = getNodeId(svgNode);
        game.play(idNumber);
    }

    function robotPlays() {
        const botNodeId = evaluator.chooseNext();
        const botSvgNode = document.querySelector(`g#id${botNodeId}`);
        document.body.addEventListener('click', onClick);
        someonePlays(botSvgNode);
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


