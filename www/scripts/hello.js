// Function to open a new tab and write "Hello, World!" in it
function openHelloWorldTab() {
    // Open a new blank tab
    const newTab = window.open('', '_blank');

    // Check if the tab was created
    if (newTab) {
        // Write "Hello, World!" to the new tab
        newTab.document.write('<h1>Hello, World!</h1>');
        newTab.document.close(); // Close the document stream
    } else {
        alert('Please allow popups for this website.');
    }
}

// Call the function to execute
openHelloWorldTab();
