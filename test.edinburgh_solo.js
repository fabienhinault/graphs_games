import {assert} from 'https://unpkg.com/chai/chai.js';

function edges_to_nextss(nVertices, edges){
    let nextss = new Array(nVertices);
    nextss.fill([], 0, nVertices);
    edges.forEach(edge => {
        nextss[edge[0]].push(edge[1]);
        nextss[edge[1]].push(edge[0]);
    });
    return nextss;
}

class MemorySequenceValueStorage {
    constructor() {
        this.values = new Map();
    }
    storeValue(sequence, value) {
        this.values.set(sequence.join(","), value);
    }
    getValue(sequence) {
        return this.values.get(sequence.join(","));
    }
    removeValue(sequence) {
        this.values.delete(sequence.join(","));
    }
}


describe('edinburgh_solo', function () {
    it('dummy', function () {
        assert.equal(1, 1);
    });

//   0--1--2--3--4--5
//               | /
//               6
    it('evaluateNext(Sync), long path', function () {
        nextss = [[1], [0, 2], [1, 3], [2, 4], [3, 5, 6], [4, 6], [4, 5]];
        const game = new Game(nextss, JSON.parse(JSON.stringify(nextss)), new MemorySequenceValueStorage(), new Clock(), null);
        game.play(6);
        game.evaluateNextsSync();
        const possibleNextsValues = game.possibleNexts.map(move => {return {move, value:game.getMoveValue(move)};});
        assert.deepEqual(possibleNextsValues, [{move: 4, value: 1000}, {move: 5, value: 980}]);
    });

    it('game.winnings', function () {
        const nextss = [[7,1],[20,0],[8,3],[19,2],[19,5],[20,16,4],[8,7],[27,9,6,0],[26,23,6,2],[10,7],[22,14,26,9],[18,12],[21,29,28,11],[21,14],[22,19,10,13],[22,16],[21,20,23,5,15],[22,18],[22,27,25,17,11],[20,29,14,26,4,3],[19,16,29,27,5,1],[16,12,28,25,24,13],[18,29,14,10,28,17,15],[16,8,24],[21,25,23],[21,18,24],[19,10,8],[20,18,7],[22,21,12],[22,20,19,12]];
        const game = new Game(nextss, JSON.parse(JSON.stringify(nextss)), new MemorySequenceValueStorage(), new Clock(), null);
        game.play("20");
        game.play("29");
        game.play("22");
        game.play("14");
        game.play("10");
        game.play("26");
        game.play("8");
        game.play("23");
        game.play("16");
        game.evaluateNextsSync();
        const possibleNextsValues = game.possibleNexts.map(move => {return {move, value:game.getMoveValue(move)};});
        assert.deepEqual(possibleNextsValues, [{move: 21, value: 1000}, {move: 5, value: 500}, {move: 15, value: 0}]);
    });

    it('game.losings', function () {
        const nextss = [[7,1],[20,0],[8,3],[19,2],[19,5],[20,16,4],[8,7],[27,9,6,0],[26,23,6,2],[10,7],[22,14,26,9],[18,12],[21,29,28,11],[21,14],[22,19,10,13],[22,16],[21,20,23,5,15],[22,18],[22,27,25,17,11],[20,29,14,26,4,3],[19,16,29,27,5,1],[16,12,28,25,24,13],[18,29,14,10,28,17,15],[16,8,24],[21,25,23],[21,18,24],[19,10,8],[20,18,7],[22,21,12],[22,20,19,12]]; 
        const game = new Game(nextss, JSON.parse(JSON.stringify(nextss)), new MemorySequenceValueStorage(), new Clock(), null);
        game.play("20");
        game.play("19");
        game.play("29");
        game.play("12");
        game.play("28");
        game.play("21");
        game.play("16");
        game.play("15");
        game.play("22");
        game.play("10");
        game.play("9");
        game.play("7");
        game.play("27");
        game.evaluateNextsSync();
        const possibleNextsValues = game.possibleNexts.map(move => {return {move, value:game.getMoveValue(move)};});
        assert.deepEqual(possibleNextsValues, [{move: 18, value: 1000}]);
        // assert.isTrue(game.losings.has('18'));
    });

    
//    0--2--3--5--6
//    | /   |
//    1     4
    it('both winning and losing', function () {
        const nextss = [[1, 2], [0, 2], [0, 1, 3], [2, 4, 5], [3], [3, 6], [5]]; 
        const game = new Game(nextss, JSON.parse(JSON.stringify(nextss)), new MemorySequenceValueStorage(), new Clock(), null);
        game.play("0");
        game.play("2");
        game.evaluateNextsSync();
        const possibleNextsValues = game.possibleNexts.map(move => {return {move, value:game.getMoveValue(move)};});
        assert.deepEqual(possibleNextsValues, [{move: 1, value: 1000}, {move: 3, value: 30}]);
        // assert.isTrue(game.losings.has("3"));
        // assert.isFalse(game.winnings.has("3"));
    });

    //   29--12--28--21--25
    //         `----'  \-24
    //                  `13
    it('game.losings', function () {
        const nextss = [[7,1],[20,0],[8,3],[19,2],[19,5],[20,16,4],[8,7],[27,9,6,0],[26,23,6,2],[10,7],[22,14,26,9],[18,12],[21,29,28,11],[21,14],[22,19,10,13],[22,16],[21,20,23,5,15],[22,18],[22,27,25,17,11],[20,29,14,26,4,3],[19,16,29,27,5,1],[16,12,28,25,24,13],[18,29,14,10,28,17,15],[16,8,24],[21,25,23],[21,18,24],[19,10,8],[20,18,7],[22,21,12],[22,20,19,12]]; 
        const game = new Game(nextss, JSON.parse(JSON.stringify(nextss)), new MemorySequenceValueStorage(), new Clock(), null);
        game.play("20");
        game.play("5");
        game.play("16");
        game.play("15");
        game.play("22");
        game.play("29");
        game.play("12");
        game.evaluateNextsSync();
        const possibleNextsValues = game.possibleNexts.map(move => {return {move, value:game.getMoveValue(move)};});
        assert.deepEqual(possibleNextsValues, [{move: 21, value: 940}, {move: 28, value: 500}, {move: 11, value: 490}]);
        // assert.isFalse(game.winnings.has("28"));
        // assert.isFalse(game.losings.has("21"));
    });

    it('evaluate', function () {
        const nextss = [[4,1],[10,6,0],[4,3],[13,12,2],[7,13,2,0],[7,6],[9,7,1,5],
                [9,6,4,11,5],[10,9],[10,7,6,12,8],[9,14,1,8],[7,14,12],[9,11,3],
                [14,4,3],[10,13,11]];
        const game = new Game(nextss, JSON.parse(JSON.stringify(nextss)), new MemorySequenceValueStorage(), new Clock(), null);
        game.sequenceValueStorage.storeValue([7,5,6,9,12,3], firstPlayer.value);
        assert.equal(game.evaluateSequence([7,5,6,9,12]), 510);
    });

    it('false winning', function () {
        const nextss = [[4,1],[10,6,0],[4,3],[13,12,2],[7,13,2,0],[7,6],[9,7,1,5],
                [9,6,4,11,5],[10,9],[10,7,6,12,8],[9,14,1,8],[7,14,12],[9,11,3],
                [14,4,3],[10,13,11]]; 
        const game = new Game(nextss, JSON.parse(JSON.stringify(nextss)), new MemorySequenceValueStorage(), new Clock(), null);
        game.sequenceValueStorage.storeValue([5,6,7,4,13,14,11,12,9,10], firstPlayer.value);
        game.play("5");
        game.play("6");
        game.play("7");
        game.play("4");
        game.play("13");
        game.play("14");
        game.play("11");
        game.play("12");
        game.play("9");
        assert.equal(game.chooseNext(), "8");
    });


});
    
