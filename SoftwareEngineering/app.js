const dec2bin = (dec) => (dec >>> 0).toString(2);
const convert = () => {
    const value = document.getElementById("decInput").value;
    if (value && !isNaN(value)){
        document.getElementById("result").textContent = `Result: ${dec2bin(value)}`

    }

    if(isNaN(value)){
        document.getElementById("result").textContent = `Result: `
        alert('Vlož číslo!')
    }

}
document.addEventListener("keyup", (e) => {
    if (e.code === "Enter") {
        convert()
    }

})

