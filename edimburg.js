document.addEventListener('DOMContentLoaded', function() {
    document.getElementById("graph0").querySelectorAll("g.node").forEach(g => {
        g.onclick = (event) => {console.log(event);}
    });
    document.body.onclick(e => console.log(e));
});
    
