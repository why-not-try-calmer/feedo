/* Context */

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

const Defaults = {
    digest_title: '',
    digest_every: 0,
    digest_at: [],
    digest_size: 10,
    digest_collapse: 0,
    blacklist: [],
    searchset: [],
    only_search_results: [],
    disable_webview: false,
    pin: false,
    follow: false,
    share_link: false
}

/* Methods */

function request_settings(token) {
    /*
    return fetch(`${Ctx.base_url}/read_settings`, {
        method: 'POST',
        headers,
        body: JSON.stringify({read_req_hash: token})
    })
    */

    return Promise.resolve(JSON.stringify({
        read_resp_settings: {
            word_matches:{
                blacklist: ['leetcode', 'dominus'],
                searchset: ['pop_os', 'haskell'],
                only_search_results: ['http://www.newcombinator.org/feed']
            },
            digest_interval: {
                digest_every_secs: 3600,
                digest_at: [[8,30], [12,0]],
            },
            digest_collapse: 3,
            digest_size: 10,
            digest_start: new Date(),
            digest_title: 'New digest available',
            disable_webview: false,
            paused: false,
            follow: true,
            pin: false,
            share_link: true
        },
        read_resp_cid: 202001010
    }))
}

function send_payload(payload) {
    return Promise.resolve(payload)
    return fetch(`${Ctx.base_url}/write_settings`, {
        method: 'POST',
        headers,
        body: JSON.stringify({
            write_req_hash: Ctx.acess_token,
            write_req_settings: payload
        })
    })
    // return Promise.resolve(JSON.stringify( {write_resp_status: 200} ))
}

function submit_listener(e) {
    const form_data = new FormData(document.forms.form)
    const form_entries = Object.fromEntries(form_data.entries())
    let payload = {
        word_matches:{
            blacklist: [],
            searchset: [],
            only_search_results: []
        },
        digest_interval: {
            digest_every_secs: null,
            digest_at: [],
        },
        digest_collapse: null,
        digest_size: null,
        digest_start: '',
        digest_title: '',
        disable_webview: false,
        paused: false,
        follow: false,
        pin: false,
        share_link: false
    }
    payload.digest_start = `${form_entries.start_yyyy}-${form_entries.start_mm}-${form_entries.start_dd}`
    for (const [k, v] of Object.entries(form_entries)) {
        switch(k) {
            case 'digest_title':
                payload.digest_title = v
                break
            case 'digest_at':
                payload.digest_interval.digest_at = v.split(' ').map(x => x.split(':').map(i => parseInt(i)))
                break
            case 'digest_every_secs':
                payload.digest_interval.digest_every_secs = parseInt(v)
                break
            case 'digest_size':
                payload.digest_size = parseInt(v)
                break
            case 'digest_collapse':
                payload.digest_collapse = parseInt(v)
                break
            case 'blacklist':
                payload.word_matches.blacklist = v.split(' ')
                break
            case 'searchset':
                payload.word_matches.searchset = v.split(' ')
                break
            case 'only_search_results':
                payload.word_matches.only_search_results = v.split(' ')
                break
            case 'disable_webview':
                payload.disable_webview = v === 'on' ? true : false
                break
            case 'follow':
                payload.follow = v === 'on' ? true : false
                break
            case 'pin':
                payload.pin = v === 'on' ? true : false
                break
            case 'share_link':
                payload.share_link = v === 'on' ? true : false
                break
        }
    }
    send_payload(payload).catch(e => console.error(e)).then(resp => console.log(resp))
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
    document.getElementById('blacklist').value = Ctx.settings.word_matches.blacklist.join(' ')
    
    const digest_at = Ctx.settings.digest_interval.digest_at.map(v => {
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
    every_s.value = Ctx.settings.digest_interval.digest_every_secs
    const [n, label] = to_human_friendly(Ctx.settings.digest_interval.digest_every_secs)
    every_s_helper.value = `${n.toString()} ${label}`

    document.getElementById('searchset').value = Ctx.settings.word_matches.searchset.join(' ')
    document.getElementById('only_search_results').value = Ctx.settings.word_matches.only_search_results.join(' ')
    
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
    const resp = await request_settings(access_token)            
    const payload = JSON.parse(resp)

    if (payload.hasOwnProperty('error')) {
        alert("Unable to authenticate your, because of this error", payload.error)
        return
    } else {
        Ctx.settings = payload.read_resp_settings
        Ctx.chat_id = payload.read_resp_cid
        Ctx.access_token = access_token       
        
        set_page()
        start_counter()
    }
}