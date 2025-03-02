function manageSlider(sliderMode) {
    const slidePanel = document.createElement("div");
    const toggleButton = document.createElement("button");
    let sliderSize = 30; // Percentage of viewport
    console.log("sliderMode", sliderMode);

    // Styling the panel based on mode
    if (sliderMode === "bottom") {
        sliderSize = 25;
        Object.assign(slidePanel.style, {
            id: "slidePanelId" + sliderMode,
            position: "fixed",
            bottom: "-" + sliderSize + "vh",
            left: "0",
            width: "100%",
            height: sliderSize + "vh",
            overflowY: "auto",
            boxShadow: "-2px 0 5px rgba(0,0,0,0.5)",
            transition: "bottom 0.5s ease-in-out",
            borderRadius: "10px", /* Rounded corners for the track */
            boxShadow: "0 2px 5px rgba(0, 0, 0, 0.2)", /* Add a subtle shadow */
            resize: "vertical", /* Allow vertical resizing */
            overflow: "auto"
        });
    } else if (sliderMode === "right") {
        Object.assign(slidePanel.style, {
            id: "slidePanelId" + sliderMode,
            zIndex: "1000",
            position: "fixed",
            top: "30%",
            right: "-" + sliderSize + "vw",
            width: sliderSize + "vw",
            height: "70%",
            overflowY: "auto",
            transition: "right 0.5s ease-in-out",
            borderRadius: "10px", /* Rounded corners for the track */
            boxShadow: "0 2px 5px rgba(0, 0, 0, 0.2)", /* Add a subtle shadow */
            resize: "horizontal", /* Allow horizontal resizing */
            overflow: "auto"
        });
    }

    // Common styling for the slide panel
    Object.assign(slidePanel.style, {
        backgroundColor: "#F7F7F7",
        textAlign: "center",
        display: "flex",
        alignItems: "center",
        flexDirection: "column"
    });
    let slideText = document.createElement("div");
    // Styling the text inside slider
    Object.assign(slideText.style, {
        verticalAlign: "top",
        marginTop: "0%",
        lineHeight: "1.2", // Adjust line height to reduce white space
        paddingLeft: "10px", // Add padding to the left
        paddingRight: "10px" // Add padding to the right
    });

    slideText.id = "slideTextId" + sliderMode;
    slideText.innerHTML = "<br><br>No file selected!";
    slidePanel.appendChild(slideText);
    document.body.appendChild(slidePanel);

    // Styling the button
    const shinyButtonStyle = {
        position: "fixed",
        transform: "translateX(-50%)",
        padding: "2px 2px",
        fontSize: "9px",
        cursor: "pointer",
        backgroundColor: "#C6C5D9",
        border: "none",
        borderRadius: "3px",
        boxShadow: "0 2px 3px rgba(0, 0, 0, 0.2)",
        transition: "background-color 0.3s, box-shadow 0.3s"
    };

    if (sliderMode == "right") {
        Object.assign(toggleButton.style, {
            ...shinyButtonStyle,
            top: "90px",
            left: "98%"
        });
    } else {
        Object.assign(toggleButton.style, {
            ...shinyButtonStyle,
            bottom: "30%",
            left: "98%"
        });
    }

    let opensign;
    let closesign;
    toggleButton.id = "toggleButtonSliderId" + sliderMode;
    if (sliderMode === "bottom") {
        opensign = "⇑";
        closesign = "⇓";
    } else {
        opensign = "⇐";
        closesign = "⇒";
    }
    toggleButton.textContent = opensign + " Species";
    document.body.appendChild(toggleButton);

    let isOpen = false;

    // Function to handle toggle button click
    function togglePanel() {
        if (isOpen) {
            toggleButton.textContent = opensign + " Species";
            if (sliderMode === "bottom") {
                slidePanel.style.bottom = "-" + sliderSize + "vh";
                document.getElementById('toggleButtonSliderIdright').style.visibility = 'visible';
            } else if (sliderMode === "right") {
                slidePanel.style.right = "-" + sliderSize + "vw";
                document.getElementById('toggleButtonSliderIdbottom').style.visibility = 'visible';
            }
            slideText.style.display = "none";
        } else {
            toggleButton.textContent = closesign + " Species";
            if (sliderMode === "bottom") {
                slidePanel.style.bottom = "0";
                document.getElementById('toggleButtonSliderIdright').style.visibility = 'hidden';
            } else if (sliderMode === "right") {
                slidePanel.style.right = "0";
                document.getElementById('toggleButtonSliderIdbottom').style.visibility = 'hidden';
            }
            slideText.style.display = "block";
        }
        isOpen = !isOpen;
    }

    toggleButton.addEventListener("click", togglePanel);
}

let sliderMode = "bottom"; // Change to "bottom / right" for side panel

document.addEventListener("DOMContentLoaded", manageSlider("right"));
document.addEventListener("DOMContentLoaded", manageSlider("bottom"));
document.getElementById('toggleButtonSliderIdright').style.visibility = 'hidden';
document.getElementById('toggleButtonSliderIdbottom').style.visibility = 'hidden';

//sliderMode = "right"; // Change to "bottom / right" for side panel
//document.addEventListener("DOMContentLoaded", manageSlider);