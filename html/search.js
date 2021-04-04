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
        let divs = document.getElementsByClassName("packages-set")
        Array.prototype.forEach.call(divs, div => {
            let children = div.children;
            var h3 = children[0];
            var ol = children[1];
            var displayed = false ;
            Array.prototype.forEach.call( ol.children, li => {
                if (li.id.toLowerCase().includes(re)) {
                    li.style.display = '';
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
        footerHandler();
    }
}
