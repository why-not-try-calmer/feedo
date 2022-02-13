
const Ctx = {
    settings: {},
    chat_id: null,
    acess_token: '',
    base_url: 'https://feedfarer-webapp.azurewebsites.net'
}

const headers = {
    'Accept': 'application/json',
    'Content-Type': 'application/json'
}

async function authorize(token) {
    return Promise.resolve(JSON.stringify({
        read_resp_settings: {
            blacklist: ['leetcode', 'dominus'],
            digest_at: [[8,30], [12,0]],
            digest_collapse: 3,
            digest_every: 3600,
            digest_size: 10,
            digest_title: 'New digest available',
            disable_webview: false,
            follow: true,
            only_search_notif: ['http://www.newcombinator.org/feed'],
            pin: false,
            search_notif: ['pop_os', 'haskell'],
            share_link: true
        },
        read_resp_cid: 202001010
    }))
    const resp = await fetch(`${base_url}/read_settings`, {
        method: 'POST',
        headers,
        body: JSON.stringify({read_req_hash: token})
    })
}

async function send_payload(payload) {
    console.log("There is your payload", payload)
    return Promise.resolve(JSON.stringify( {write_resp_status: 200} ))
    const resp = await fetch(`${base_url}/write_settings`, {
        method: 'POST',
        headers,
        body: JSON.stringify({
            write_req_hash: Ctx.acess_token,
            write_req_settings: payload
        })
    })
}

function submit_listener(e) {
    const _data = new FormData(document.forms.form)
    let digest_start = ['','','']
    let payload = {}
    for (const [k, v] of Object.fromEntries(_data.entries()).entries()) {
        console.log(k, v)
        switch (k) {
            case 'blacklist':
                payload.blacklist = v
                break
            case 'collapse':
                payload.collapse = v
                break
            case 'every_n':
                payload.every = v
                break
            case 'follow':
                payload.follow = v
                break
            case 'search_notif':
                payload = v
                break
            case 'share_link':
                payload.share_link = v
                break
            case 'size':
                payload.size = v
                break
            case 'start_yyy':
                digest_start[2] = v 
                break
            case 'start_mm':
                digest_start[1] = v
                break
            case 'start_dd':
                digest_start[0] = v
                break
            case 'title':
                payload.title = v
                break
        }
    }
    payload.digest_start = digest_start.join('')
    const ready = JSON.stringify(payload)
    send_payload(ready).catch(e => console.error(e)).then(resp => console.log(resp))
    e.preventDefault()
}

function start_counter(counter = 300) {
    const tgt = document.getElementById('time_left')
    const handler = () => {
        counter--
        if (counter === 0) tgt.innerHTML = 'Time elapsed!'
        else tgt.innerHTML = counter.toString()
    }
    setTimeout(() => counter * 1000)
    setInterval(handler, 1000)
}

function to_human_friendly(n) {
    let m = null
    if (n % 86400 === 0) {
        m = n / 86400
        return [m, m == 1 ? 'day' : 'days']
    }
    if (n % 3600 === 0) {
        m = n / 3600
        return [m, m == 1 ? 'hour' : 'hours']
    }
    if (n % 60 === 0) {
        m = n / 60 
        return [m, m == 1 ? 'minute' : 'minutes']
    }
    return [n, 'seconds']
}

function helper_listener(e) {
    const input = document.getElementById('digest_every_secs').value
    const rewrite = i => {
        const [n, label] = to_human_friendly(i)
        document.getElementById('digest_every_secs_helper').value = `${n.toString()} ${label}`
    }
    setTimeout(() => rewrite(input), 350)
    e.preventDefault()
}

const Defaults = {
    digest_title: '',
    digest_every: 0,
    digest_at: [],
    digest_size: 10,
    digest_collapse: 0,
    blacklist: [],
    search_notif: [],
    only_search_notif: [],
    disable_webview: false,
    pin: false,
    follow: false,
    share_link: false
}

function reset_field(n){
    const field_name = n === 'digest_every' ? 'digest_every_secs' : n
    const field = document.getElementById(field_name)
    const def = Defaults[field_name]
    if (['share_link', 'disable_webview', 'follow', 'pin'].includes(field_name)) {
        field.checked = def
        return
    }
    field.value = def
}

function asssign_from_Ctx() {
    document.getElementById('blacklist').value = Ctx.settings.blacklist.join(' ')
    
    const digest_at = Ctx.settings.digest_at.map(v => {
        let [h, m] = v
        if (h < 10) h = "0" + h
        if (m < 10) m = "0" + m
        return `${h.toString()}:${m.toString()}`
    }).join(' ')
    document.getElementById('digest_at').value = digest_at
    document.getElementById('digest_size').value = Ctx.settings.digest_size
    document.getElementById('digest_title').value = Ctx.settings.digest_title
    document.getElementById('digest_collapse').value = Ctx.settings.digest_collapse
    
    every_s_helper = document.getElementById('digest_every_secs_helper')
    every_s = document.getElementById('digest_every_secs')
    every_s.value = Ctx.settings.digest_every
    const [n, label] = to_human_friendly(Ctx.settings.digest_every)
    every_s_helper.value = `${n.toString()} ${label}`

    document.getElementById('search_notif').value = Ctx.settings.search_notif.join(' ')
    document.getElementById('only_search_notif').value = Ctx.settings.only_search_notif.join(' ')
    
    document.getElementById('share_link').checked = Ctx.settings.share_link
    document.getElementById('disable_webview').checked = Ctx.settings.disable_webview
    document.getElementById('follow').checked = Ctx.settings.follow
    document.getElementById('pin').checked = Ctx.settings.pin
}

function set_page() {
    const form = document.getElementById('form')
    form.addEventListener('submit', submit_listener)
    const resetters = document.getElementsByClassName('field_resetter')
    Array.from(resetters).forEach(btn => btn.addEventListener('click', (e) => { reset_field(e.target.name); e.preventDefault() })) 
    const reset_all = document.getElementById('reset_all')
    reset_all.addEventListener('click', (e) => { 
        Object.keys(Defaults).forEach(k => {
            reset_field(k)
        })
        e.preventDefault() 
    })
    const reloader = document.getElementById('reload')
    reloader.addEventListener('click', (e) => {
        asssign_from_Ctx()
        e.preventDefault()
    })

    document.getElementById('access_token').innerHTML = "Access token: " + Ctx.access_token
    document.getElementById('chat_id').innerHTML = "Chat Id: " + Ctx.chat_id
    document.getElementById('showcase').innerHTML = JSON.stringify(Ctx.settings)
    
    every_s = document.getElementById('digest_every_secs')    
    every_s.addEventListener('keyup', helper_listener)

    asssign_from_Ctx()
}

window.onload = async () => {
    const params = new Proxy(new URLSearchParams(window.location.search), {
        get: (searchParams, prop) => searchParams.get(prop),
    });
    const access_token = params.access_token
    const resp = await authorize(access_token)            
    const payload = JSON.parse(resp)

    if (payload.hasOwnProperty('error')) {
        alert("Unable to authenticate your, because of this error", payload.error)
        return;
    } else {
        Ctx.settings = payload.read_resp_settings
        Ctx.chat_id = payload.read_resp_cid
        Ctx.access_token = access_token       
        
        set_page()
        start_counter()
    }
}