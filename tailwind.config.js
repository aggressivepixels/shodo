/** @type {import('tailwindcss').Config} */

module.exports = {
  content: ["./src/**/*.elm", "index.html"],
  theme: {
    extend: {
      fontFamily: {
        "klee-one": ["Klee One", "cursive"],
      },
    },
  },
  plugins: [],
};
