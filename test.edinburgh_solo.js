const assert = chai.assert;
describe('edinburgh_solo', function () {
    it('dummy', function () {
        assert.equal(1, 1);
    });


//   0--1--2--3--4--5
//               |  |
//               6--'
    it('takeWinning, long path', function () {
        const game = new Game([["1"], ["0", "2"], ["1", "3"], ["2", "4"], ["3", "5", "6"], ["4", "6"], ["5"]], null);
        game.moves.push("6");
        game.takeWinnings(["1"], 0);
        assert.deepEqual(game.winnings, new Set(["0", "2", "4"]));
        assert.deepEqual(game.losings, new Set(["1", "3"]));
    });

    it('game.winnings', function () {
        const game = new Game([["7","1"],["20","0"],["8","3"],["19","2"],["19","5"],["20","16","4"],["8","7"],["27","9","6","0"],["26","23","6","2"],["10","7"],["22","14","26","9"],["18","12"],["21","29","28","11"],["21","14"],["22","19","10","13"],["22","16"],["21","20","23","5","15"],["22","18"],["22","27","25","17","11"],["20","29","14","26","4","3"],["19","16","29","27","5","1"],["16","12","28","25","24","13"],["18","29","14","10","28","17","15"],["16","8","24"],["21","25","23"],["21","18","24"],["19","10","8"],["20","18","7"],["22","21","12"],["22","20","19","12"]],
                            null);
        game.play("20");
        game.play("29");
        game.play("22");
        game.play("14");
        game.play("10");
        game.play("26");
        game.play("8");
        game.play("23");
        game.play("16");
        assert.isTrue(game.winnings.has("15"));
    });

    it('game.losings', function () {
        const game = new Game([["7","1"],["20","0"],["8","3"],["19","2"],["19","5"],["20","16","4"],["8","7"],["27","9","6","0"],["26","23","6","2"],["10","7"],["22","14","26","9"],["18","12"],["21","29","28","11"],["21","14"],["22","19","10","13"],["22","16"],["21","20","23","5","15"],["22","18"],["22","27","25","17","11"],["20","29","14","26","4","3"],["19","16","29","27","5","1"],["16","12","28","25","24","13"],["18","29","14","10","28","17","15"],["16","8","24"],["21","25","23"],["21","18","24"],["19","10","8"],["20","18","7"],["22","21","12"],["22","20","19","12"]],
                            null);
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
        assert.isTrue(game.losings.has('18'));
    });

    it('both winning and losing', function () {
        const game = new Game([["1", "2"], ["0", "2"], ["0", "1", "3"], ["2", "4", "5"], ["3"], ["3", "6"], ["5"]], null);
        game.play("0");
        game.play("2");
        assert.isTrue(game.losings.has("3"));
        assert.isFalse(game.winnings.has("3"));
    });


    it('game.losings', function () {
        const game = new Game([["7","1"],["20","0"],["8","3"],["19","2"],["19","5"],["20","16","4"],["8","7"],["27","9","6","0"],["26","23","6","2"],["10","7"],["22","14","26","9"],["18","12"],["21","29","28","11"],["21","14"],["22","19","10","13"],["22","16"],["21","20","23","5","15"],["22","18"],["22","27","25","17","11"],["20","29","14","26","4","3"],["19","16","29","27","5","1"],["16","12","28","25","24","13"],["18","29","14","10","28","17","15"],["16","8","24"],["21","25","23"],["21","18","24"],["19","10","8"],["20","18","7"],["22","21","12"],["22","20","19","12"]],
                            null);
        game.play("20");
        game.play("5");
        game.play("16");
        game.play("15");
        game.play("22");
        game.play("29");
        game.play("12");
        assert.isTrue(game.winnings.has("28"));
        assert.isTrue(game.losings.has("21"));
    });


});
    
