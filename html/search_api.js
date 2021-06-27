/* Arguments for API requests */
var last_id = 0;
var starts_with = ".";
var pattern = "~";

const createApiArgument = () => {
    return last_id + "+" + starts_with + "+" + pattern;
}

/* Variables */
var main_div;
var reversed_path = window.location.pathname.split('/').reverse();
var filename = reversed_path[0];


/* Observer that calls API for new entries when reaching the bottom of the page*/
var load_div = document.createElement("div");
load_div.setAttribute("id", "load_div");

var observer = new IntersectionObserver(function(entries) {

    if(entries[0].isIntersecting === true){

        main_div.removeChild(load_div);
        sendRequest().then(function(added){
            if(added){
                last_id = last_id + 50;
                main_div.appendChild(load_div);
            }
        })
    }
}, { threshold: [1] });

const insert_packages = packages => {
    var first_letter = packages[0].name[0].toLowerCase();
    document.getElementById("name-"+first_letter).style.display= "";
        
    for(i in packages){
        var pkg = document.createElement("li");
        pkg.setAttribute("class", "package");
            
        var pkg_name = document.createElement("a");
        pkg_name.setAttribute("class", "digodoc-opam");
        pkg_name.setAttribute("href", packages[i].path);
        var name = document.createElement("code");
        name.innerHTML = packages[i].name;
        pkg_name.appendChild(name);
            
        pkg.appendChild(pkg_name);
        pkg.innerHTML += ' ' + packages[i].synopsis;

        if(first_letter != packages[i].name[0].toLowerCase()){
            first_letter = packages[i].name[0].toLowerCase();
            document.getElementById("name-"+first_letter).style.display= "";
        }
        var pkg_set = document.getElementById("packages-" + first_letter);
        pkg_set.appendChild(pkg);
    }
}

const insert_libraries = libraries => {
    var first_letter = libraries[0].name[0].toLowerCase();
    document.getElementById("name-"+first_letter).style.display= "";
        
    for(i in libraries){
        var lib = document.createElement("li");
        lib.setAttribute("class", "package");
            
        var lib_name = document.createElement("a");
        lib_name.setAttribute("class", "digodoc-lib");
        lib_name.setAttribute("href", libraries[i].path);
        var name = document.createElement("code");
        name.innerHTML = libraries[i].name;
        lib_name.appendChild(name);
            
        lib.appendChild(lib_name);
        lib.innerHTML += ' in opam ';

        var lib_opam = document.createElement("a");
        lib_opam.setAttribute("class", "digodoc-opam");
        lib_opam.setAttribute("href", libraries[i].opam_path);
        lib_opam.innerHTML = libraries[i].opam;

        lib.appendChild(lib_opam);

        if(first_letter != libraries[i].name[0].toLowerCase()){
            first_letter = libraries[i].name[0].toLowerCase();
            document.getElementById("name-"+first_letter).style.display= "";
        }

        var lib_set = document.getElementById("packages-" + first_letter);
        lib_set.appendChild(lib);
    }
}


const insert_modules = modules => {
    var first_letter = modules[0].name[0].toLowerCase();
    document.getElementById("name-"+first_letter).style.display= "";
        
    for(i in modules){
        var mod = document.createElement("li");
        mod.setAttribute("class", "package");
            
        var mod_name = document.createElement("a");
        mod_name.setAttribute("href", modules[i].path);
        var name = document.createElement("code");
        name.innerHTML = modules[i].name;
        mod_name.appendChild(name);
            
        mod.appendChild(mod_name);
        mod.innerHTML += ' in opam ';

        var mod_opam = document.createElement("a");
        mod_opam.setAttribute("class", "digodoc-opam");
        mod_opam.setAttribute("href", modules[i].opam_path);
        mod_opam.innerHTML = modules[i].opam;

        mod.appendChild(mod_opam);
        if(modules[i].libs.length > 0){
            mod.innerHTML += ' in libs ';
        }
            
        for(j in modules[i].libs){
            var mod_libs = document.createElement("a");
            mod_libs.setAttribute("class", "digodoc-lib");
            mod_libs.setAttribute("href", modules[i].libs[j][1]);
            mod_libs.innerHTML = modules[i].libs[j][0];
            mod.appendChild(mod_libs);
            if(j+1 < modules[i].libs.length){
                mod.innerHTML += ", ";
            }
        }

        if(first_letter != modules[i].name[0].toLowerCase()){
            first_letter = modules[i].name[0].toLowerCase();
            document.getElementById("name-"+first_letter).style.display= "";
        }
        var mod_set = document.getElementById("packages-" + first_letter);
        mod_set.appendChild(mod);
    }
}

const insert_metas = metas => {
    var first_letter = metas[0].name[0].toLowerCase();
    document.getElementById("name-"+first_letter).style.display= "";
        
    for(i in metas){
        var meta = document.createElement("li");
        meta.setAttribute("class", "package");
            
        var meta_name = document.createElement("a");
        meta_name.setAttribute("href", metas[i].path);
        var name = document.createElement("code");
        name.innerHTML = metas[i].name;
        meta_name.appendChild(name);
            
        meta.appendChild(meta_name);
        meta.innerHTML += ' in opam ';

        var meta_opam = document.createElement("a");
        meta_opam.setAttribute("class", "digodoc-opam");
        meta_opam.setAttribute("href", metas[i].opam_path);
        meta_opam.innerHTML = metas[i].opam;

        meta.appendChild(meta_opam);
        
        if(first_letter != metas[i].name[0].toLowerCase()){
            first_letter = metas[i].name[0].toLowerCase();
            document.getElementById("name-"+first_letter).style.display= "";
        }
        var meta_set = document.getElementById("packages-" + first_letter);
        meta_set.appendChild(meta);
    }
}

const insert_sources = sources => {
    var first_letter = sources[0].name[0].toLowerCase();
    document.getElementById("name-"+first_letter).style.display= "";
        
    for(i in sources){
        var src = document.createElement("li");
        src.setAttribute("class", "package");
            
        var src_name = document.createElement("a");
        src_name.setAttribute("href", sources[i].path);
        var name = document.createElement("code");
        name.innerHTML = sources[i].name;
        src_name.appendChild(name);
            
        src.appendChild(src_name);
        src.innerHTML += ' in opam ';

        var src_opam = document.createElement("a");
        src_opam.setAttribute("class", "digodoc-opam");
        src_opam.setAttribute("href", sources[i].opam_path);
        src_opam.innerHTML = sources[i].opam;

        src.appendChild(src_opam);
        
        if(first_letter != sources[i].name[0].toLowerCase()){
            first_letter = sources[i].name[0].toLowerCase();
            document.getElementById("name-"+first_letter).style.display= "";
        }
        var src_set = document.getElementById("packages-" + first_letter);
        src_set.appendChild(src);
    }
}

const getEntriesNumber = async () => {
    var entry;
    switch (filename) {
        case "index.html":
            entry = "packages";
            break;
        case "modules.html":
            entry = "modules";
            break;
        case "libraries.html":
            entry = "libraries";
            break;
        case "metas.html":
            entry = "metas";
            break;
        case "sources.html":
            entry = "sources";
            break;
        default:
            break;
    }
    const response = await fetch('http://localhost:11001/command/count+'+ entry + "/" + createApiArgument());
    const results = await response.json();
    var indicator = document.getElementById("item-number");
    indicator.innerHTML = results.result + " " + entry;
}

const sendRequest = async () => {
    var response;
    switch (filename) {
        case "index.html":
            response = await fetch('http://localhost:11001/packages/'+ createApiArgument());
            break;
        case "modules.html":
            response = await fetch('http://localhost:11001/modules/'+ createApiArgument());
            break;
        case "libraries.html":
            response = await fetch('http://localhost:11001/libraries/'+ createApiArgument());
            break;
        case "metas.html":
            response = await fetch('http://localhost:11001/metas/'+ createApiArgument());
            break;
        case "sources.html":
            response = await fetch('http://localhost:11001/sources/'+ createApiArgument());
            break;
        default:
            break;
    }
    
    const results = await response.json(); //extract JSON from the http response
    if (results.length == 0){
        return false;
    }
    else{
        switch (filename) {
            case "index.html":
                insert_packages(results);
                break;
            case "modules.html":
                insert_modules(results);
                break;
            case "libraries.html":
                insert_libraries(results);
                break;
            case "metas.html":
                insert_metas(results)
                break;
            case "sources.html":
                insert_sources(results);
                break;
            default:
                break;
        }
        return true;
    }
}


const clear_index_page = () => {
    if(document.getElementById("load_div")){
        main_div.removeChild(load_div);
    }
    for (let index = 48; index < 58; index++) {
        var set = document.getElementById("packages-"+String.fromCharCode(index));
        if(set){
            set.innerHTML="";
            var title = document.getElementById("name-"+String.fromCharCode(index));
            title.style.display= "none";
        }
    }
    for (let index = 97; index < 123; index++) {
        var set = document.getElementById("packages-"+String.fromCharCode(index));
        if(set){
            set.innerHTML="";
            var title = document.getElementById("name-"+String.fromCharCode(index));
            title.style.display= "none";
        }
    }
}

const update_index_page = () => {
    sendRequest().then(function(added){
        if(added){
            last_id = last_id + 50;
            main_div.appendChild(load_div);
        }
        getEntriesNumber();
        footerHandler();
    });
}

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

    if (reversed_path[1] == 'html' && filename != 'about.html') {
        main_div = document.getElementById("by-name");
        
        for (let index = 48; index < 58; index++) {
            var title = document.getElementById("name-"+String.fromCharCode(index));
            if(title) title.style.display= "none";
        }
            

        for (let index = 97; index < 123; index++) {
            var title = document.getElementById("name-"+String.fromCharCode(index));
            if(title) title.style.display= "none";
        }

        sendRequest().then(function(_){ 
            getEntriesNumber();
            last_id = last_id + 50;
            main_div.appendChild(load_div);
            observer.observe(document.querySelector("#load_div"));
        });
    }

    
    var search = document.getElementById("search")

    search.onkeyup = () => {
        let re = search.value;

        if (reversed_path[1] == 'html'  && filename != 'about.html') {
            clear_index_page();
    
            const input = re.trim();
            if(input.length > 0){
                pattern = input;
            }
            else {
                pattern = "~";
            }
            last_id = 0;
            
            update_index_page ();
        } 
    }
}

const set_start_letter = letter => {
    starts_with = letter;
    document.getElementById("search").value = "";
    last_id = 0;
    pattern = "~";
    clear_index_page ();
    update_index_page ();
}