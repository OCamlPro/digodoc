/*
This code assumes that:
* "search" is the id of the search input
* every group of packages with a common first letter is organized as:
  * a [div] with class="packages-set" containing
    * an [h3] with the letter
    * an [ol] containing only a list of [li], 
         the search is performed on the [id] of the [li]
*/

window.onload = () => {
    footerHandler();

    let name = document.getElementById("search")

    name.onkeyup = () => {
        let re = name.value.toLowerCase();
        let divs = document.getElementsByClassName("packages-set");
        let cpt = 0;
        Array.prototype.forEach.call(divs, div => {
            let children = div.children;
            var h3 = children[0];
            var ol = children[1];
            var displayed = false ;
            Array.prototype.forEach.call( ol.children, li => {
                if (li.id.toLowerCase().includes(re)) {
                    li.style.display = '';
                    cpt++;
                    displayed = true;
                } else {
                    li.style.display = "none";
                }});
            if( displayed ){
                h3.style.display = ""
            } else {
                h3.style.display = "none"
            }
        });
        let items_nbr = document.getElementById("item-number");
        let content = items_nbr.innerHTML.split(' ');
        content[0] = cpt;
        items_nbr.innerHTML = content.join(' ');
        footerHandler();
    }
}
