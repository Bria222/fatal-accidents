/* -------- Dark Mode Toggle -------- */
const toggleBtn = document.getElementById('darkToggle')
const root = document.documentElement

// Load saved theme
if (localStorage.getItem('theme')) {
  root.dataset.theme = localStorage.getItem('theme')
}

// Toggle on click
toggleBtn.onclick = () => {
  const newTheme = root.dataset.theme === 'light' ? 'dark' : 'light'
  root.dataset.theme = newTheme
  localStorage.setItem('theme', newTheme)
}

/* -------- Reveal on Scroll -------- */
function reveal() {
  document.querySelectorAll('.reveal').forEach((el) => {
    const rect = el.getBoundingClientRect()
    if (rect.top < window.innerHeight - 100) {
      el.classList.add('visible')
    }
  })
}
window.addEventListener('scroll', reveal)
window.addEventListener('load', reveal)

/* -------- KPI Counter Animation -------- */
document.querySelectorAll('.kpi').forEach((kpi) => {
  const target = +kpi.dataset.target
  let value = 0

  const update = () => {
    value += target / 100
    if (value < target) {
      kpi.textContent = Math.floor(value)
      requestAnimationFrame(update)
    } else {
      kpi.textContent = target
    }
  }
  update()
})
