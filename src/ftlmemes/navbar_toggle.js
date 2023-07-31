const toggle = document.querySelector('#navbar-toggle');
const menu = document.querySelector('#navbar');

function toggleNavbar() {
  menu.classList.toggle('is-open');
}

toggle.addEventListener('click', function(_e) {
    toggleNavbar();
});


window.navbarToggle = {
    toggleNavbar: toggleNavbar
};
