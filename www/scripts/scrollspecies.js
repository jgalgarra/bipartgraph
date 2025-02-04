const sliderMode = "right"; // Change to "bottom / right" for side panel
const slideText = document.createElement("div");
let showButton = true;

        document.addEventListener("DOMContentLoaded", function () {
            const slidePanel = document.createElement("div");
            const toggleButton = document.createElement("button");
            const sliderSize = 35; // Percentage of viewport
            
            // Styling the panel based on mode
            if (sliderMode === "bottom") {
                Object.assign(slidePanel.style,{ 
                    position: "fixed",
                    bottom: "-" + sliderSize + "vh",
                    left: "0",
                    width: "100%",
                    height: sliderSize + "vh",
                    overflowY: "auto",
                    boxShadow : "-2px 0 5px rgba(0,0,0,0.5)",
                    transition: "bottom 0.5s ease-in-out",
                });
            } else if (sliderMode === "right") {
                Object.assign(slidePanel.style, {
                    zIndex: "1000",
                    position: "fixed",
                    top: "30%",
                    right: "-" + sliderSize + "vw",
                    width: sliderSize + "vw",
                    height: "70%",
                    justifyContent: "top",
                    overflowY: "auto",
                    transition: "right 0.5s ease-in-out",
                    borderRadius: "10px", /* Rounded corners for the track */
                    boxShadow: "0 2px 5px rgba(0, 0, 0, 0.2)" /* Add a subtle shadow */
                });
            }
            
            Object.assign(slidePanel.style, {
                backgroundColor: "#F7FEFF",
                textAlign: "center",
                display: "flex",
                alignItems: "center",
                justifyContent: "top",
                flexDirection: "column"
            });
            
            // Styling the text inside slider
             Object.assign(slideText.style, {
                verticalAlign: "top",
                marginTop:"0%"
            }); 
            slideText.id = "slideTextId";
            slideText.innerHTML = "<br><br>No file selected!";
            slidePanel.appendChild(slideText);
            document.body.appendChild(slidePanel);
            // Styling the button
            Object.assign(toggleButton.style, {
                position: "fixed",
                top: "90px",
                left: "95%",
                transform: "translateX(-50%)",
                padding: "1px 1px",
                fontSize: "9px",
                cursor: "pointer",
            });
            toggleButton.id = "toggleButtonSliderId";
            toggleButton.textContent = "<< Species";
            document.body.appendChild(toggleButton);
            
            let isOpen = false;

            toggleButton.addEventListener("click", function () {
                if (isOpen) {
                    toggleButton.textContent = "<< Species";
                    if (sliderMode === "bottom") {
                        slidePanel.style.bottom = "-" + sliderSize + "vh";
                    } else if (sliderMode === "right") {
                        slidePanel.style.right = "-" + sliderSize + "vw";
                    }
                    slideText.style.display = "none";
                } else {
                    toggleButton.textContent = ">> Species";
                    if (sliderMode === "bottom") {
                        slidePanel.style.bottom = "0";
                    } else if (sliderMode === "right") {
                        slidePanel.style.right = "0";
                    }
                    slideText.style.display = "block";
                }
                isOpen = !isOpen;
            });
             
        if (showButton)
            toggleButton.style.display = "block"; // Show the button
        else
            toggleButton.style.display = "none";  // Hide the button
        });