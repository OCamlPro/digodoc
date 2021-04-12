var footerHandler = () => {
    let footer = document.getElementById("footer");

    footer.style.position = 'relative' 

    if (window.innerHeight <= document.body.clientHeight){
      footer.style.position = 'relative' 
    } else{
      footer.style.position = 'fixed' 
    } 
}

window.onresize = footerHandler;

var path_reversed = window.location.pathname.split('/').reverse();
var dirname = path_reversed[1];
if (dirname != "html"){
  window.onload = footerHandler;
} else {
  var filename = path_reversed[0];
  switch (filename) {
    case "about.html":
      document.getElementById("about-item").className = "active";
      break;
    case "index.html":
      document.getElementById("packages-item").className = "active";
      break;
    case "libraries.html":
      document.getElementById("libraries-item").className = "active";
      break;
    case "metas.html":
      document.getElementById("metas-item").className = "active";
      break;  
    case "modules.html":
      document.getElementById("modules-item").className = "active";
      break;
    case "sources.html":
      document.getElementById("sources-item").className = "active";
      break;  
    default:
      break;
  }

}
