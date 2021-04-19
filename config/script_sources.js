var footerHandler = () => {
    let footer = document.getElementById("footer");

    footer.style.position = 'relative' 

    if (window.innerHeight <= document.body.clientHeight){
      footer.style.position = 'relative' 
    } else{
      footer.style.position = 'fixed' 
    } 
  }

  window.onload = footerHandler;
  window.onresize = footerHandler;

  document.getElementById("search").style.display = "none";