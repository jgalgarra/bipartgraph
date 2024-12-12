  // Get references to the slider and the SVG element
  const zoomSlider = document.getElementById("zoomSlider");
  const svgPlot = document.getElementById("svgPlot");
  const zoomValue = document.getElementById("zoomValue");

  // Function to update the viewBox based on zoom level
  function updateZoomLevel() {
    const zoom = parseFloat(zoomSlider.value);
    zoomValue.textContent = zoom.toFixed(1);  // Display current zoom level
    
    // Calculate the new width and height of the viewBox based on zoom
    const viewBoxSize = 100 / zoom;
    
    // Update the viewBox: This ensures the viewBox starts at the top-left (0, 0)
    svgPlot.setAttribute("viewBox", `0 0 ${viewBoxSize} ${viewBoxSize}`);
  }

  // Listen to the slider change event
  zoomSlider.addEventListener("input", updateZoomLevel);

  // Initialize the zoom level
  updateZoomLevel();

  window.onload = (event) => {
    updateZoomLevel();
  };
