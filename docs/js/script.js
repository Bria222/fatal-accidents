const toggleBtn = document.getElementById('darkToggle')
const root = document.documentElement

if (localStorage.getItem('theme')) {
  root.dataset.theme = localStorage.getItem('theme')
}

toggleBtn.onclick = () => {
  const newTheme = root.dataset.theme === 'light' ? 'dark' : 'light'
  root.dataset.theme = newTheme
  localStorage.setItem('theme', newTheme)
}

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
