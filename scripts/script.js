// Get the modal
var modal = document.getElementById("myModal");

// Get the image and insert it inside the modal - use its "alt" text as a caption
var modalImg = document.getElementById("img01");
var captionText = document.getElementById("caption");
var downloadLink = document.querySelector('.download-link');

// Use a class to select all  images
var images = document.querySelectorAll('.thumbnail');

images.forEach(function(image) {
  image.onclick = function(){
    modal.style.display = "flex"; // Change display to flex when image is clicked
    modalImg.src = this.src;
    captionText.innerHTML = this.alt;

    // Update the download link to point to the .tif file instead of the .jpg
    var tifSrc = this.src.replace(".jpg", ".tif");
    downloadLink.href = tifSrc;
  }
});

// Get the <span> element that closes the modal
var span = document.getElementsByClassName("close")[0];

// When the user clicks on <span> (x), close the modal
span.onclick = function() { 
  modal.style.display = "none"; // Hide the modal again
}

// When the user clicks anywhere outside of the modal content (the image, header, or download link), close the modal
modal.onclick = function(event) {
if (event.target == modal) {
modal.style.display = "none";
}
}

// Add event listeners to the modal-trigger elements
document.querySelectorAll('.modal-trigger').forEach(function(element) {
element.addEventListener('click', function() {
    var jpgSrc = this.getAttribute('data-img-src');
    var fullJpgSrc = 'images/' + jpgSrc;
    var tifSrc = fullJpgSrc.replace(".jpg", ".tif");

    modal.style.display = "flex";
    modalImg.src = fullJpgSrc;
    captionText.innerHTML = this.innerText;
    downloadLink.href = tifSrc;
});
});

window.addEventListener('scroll', function() {
var element = document.getElementById('key');
element.style.right = 10 - window.pageXOffset + 'px';
});
