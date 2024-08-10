"use strict";

import { voronoize } from './voronoize.js';

export class View {
    constructor(game) {
        this.game = game;
        this.onPlayedFn = this.onPlayed.bind(this);
        this.onGameOverFn = this.onGameOver.bind(this);
        this.game.addPlayedListener(this.onPlayedFn);
        this.game.addGameOverListener(this.onGameOverFn);
        this.currentSvgNode;
        this.previousSvgNode;
        document.querySelectorAll('span#won > img').forEach(img => img.setAttribute('class', 'playing'));
        const graphElement = document.querySelector('#graph0');
        const parentSvg = graphElement.closest('svg');
        this.moveEdgesLast();
        voronoize(parentSvg, graphElement);
    }

    getNodeId(svgNode) {
        // remove "id"
        return Number(svgNode.id.substring(2));
    }

    moveEdgesLast() {
        document.querySelectorAll('#graph0 > g.edge').forEach(g => {
            g.parentElement.appendChild(g);
        });
    }

    stopListening() {
        this.game.dispatcher.removeEventListener("played", this.onPlayedFn);
        this.game.dispatcher.removeEventListener("game over", this.onGameOverFn);
    }

    onGameOver(evt) {
        const winnerName = evt.detail.winner;
        const klass = `${winnerName}_won`;
        document.querySelectorAll('span#won > img').forEach(img => img.setAttribute('class', klass));
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
        document.querySelectorAll(`g._${this.getNodeId(this.previousSvgNode)}_`).forEach(g => {
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
