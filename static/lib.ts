/* Context */

interface WordMatches {
    blacklist: string[]
    searchset: string[]
    only_search_results: string[]
}

interface DigestInterval {
    digest_every_secs?: number
    digest_at: any[]
}

interface Settings {
    digest_interval?: DigestInterval
    digest_collapse?: number
    digest_size: number
    digest_start?: Date
    digest_title: string
    disable_web_view: boolean
    forward_to_admins: boolean
    pagination: boolean
    paused: boolean
    pin: boolean
    share_link: boolean
    word_matches: WordMatches
    digest_no_collapse: string[]
}

const Ctx = {
    settings: {},
    chat_id: null,
    acess_token: '',
    base_url: 'https://feedo.cloudns.ph',
    headers: {
        'Accept': 'application/json',
        'Content-Type': 'application/json'
    },
    request_settings: function (token: string) {
        return fetch(`${this.base_url}/read_settings`, {
            method: 'POST',
            headers: this.headers,
            body: JSON.stringify({ read_req_hash: token })
        })
    },
    send_payload: function (settings: Settings) {
        return fetch(`${this.base_url}/write_settings`, {
            method: 'POST',
            headers: this.headers,
            body: JSON.stringify(settings)
        })
    }
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
    disable_web_view: false,
    pin: false,
    share_link: false,
    pagination: false
}

function submit_listener(e: Event) {
    e.preventDefault()
    const form_data = new FormData(document.forms[0])
    const form_entries = Object.fromEntries(form_data.entries())
    const settings: Settings = {
        word_matches: {
            blacklist: [],
            searchset: [],
            only_search_results: []
        },
        digest_interval: {
            digest_every_secs: undefined,
            digest_at: [],
        },
        digest_collapse: undefined,
        digest_size: 0,
        digest_start: undefined,
        digest_title: '',
        disable_web_view: false,
        paused: false,
        forward_to_admins: false,
        digest_no_collapse: [],
        pin: false,
        share_link: false,
        pagination: false
    }
    const mb_digest_start = `${form_entries.start_yyyy}-${form_entries.start_mm}-${form_entries.start_dd}`
    settings.digest_start = mb_digest_start.length > 3 ? mb_digest_start : undefined
    let parsed = null
    for (const [k, v] of Object.entries(form_entries)) {
        switch (k) {
            case 'digest_title':
                settings.digest_title = v
                break
            case 'digest_at':
                settings.digest_interval.digest_at = v.split(' ').map(x => x.split(':').map(i => parseInt(i))) || null
                break
            case 'digest_every_secs':
                parsed = parseInt(v)
                settings.digest_interval.digest_every_secs = parsed > 0 ? parsed : null
                break
            case 'digest_size':
                settings.digest_size = parseInt(v) || null
                break
            case 'digest_collapse':
                parsed = parseInt(v)
                settings.digest_collapse = parsed > 0 ? parsed : null
                break
            case 'blacklist':
                settings.word_matches.blacklist = v.split(' ')
                break
            case 'searchset':
                settings.word_matches.searchset = v.split(' ')
                break
            case 'only_search_results':
                settings.word_matches.only_search_results = v.split(' ')
                break
            case 'disable_web_view':
                settings.disable_web_view = v === 'on' ? true : false
                break
            case 'follow':
                settings.follow = v === 'on' ? true : false
                break
            case 'pin':
                settings.pin = v === 'on' ? true : false
                break
            case 'share_link':
                settings.share_link = v === 'on' ? true : false
                break
            case 'pagination':
                settings.pagination = v === 'on' ? true : false
        }
    }
    const preflight = { write_req_hash: Ctx.access_token, write_req_settings: settings }
    Ctx.send_payload(preflight).then(resp => resp.json()).then(res => {
        if (confirm(res.write_resp_checkout)) {
            const confirmed = Object.assign({ write_req_confirm: true }, preflight)
            return Ctx.send_payload(confirmed).then(resp => resp.json()).then(res => {
                if (res.write_resp_status === 200) {
                    const popup = document.getElementById('success')
                    popup.style.display = 'block'
                }
                else {
                    const popup = document.getElementById('failure')
                    popup.style.display = 'block'
                }
            })
        }
    }).catch(e => alert('Failed for this reason', e))
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
    e.preventDefault()
    const input = document.getElementById('digest_every_secs').value
    const rewrite = i => {
        const [n, label] = to_human_friendly(i)
        document.getElementById('digest_every_secs_helper').value = `${n.toString()} ${label}`
    }
    setTimeout(() => rewrite(input), 350)
}

function reset_field(n) {
    const field_name = n === 'digest_every' ? 'digest_every_secs' : n
    const field = document.getElementById(field_name)
    const def = Defaults[field_name]
    if (['share_link', 'disable_web_view', 'follow', 'pin', 'pagination'].includes(field_name)) {
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

    document.getElementById('pagination').checked = Ctx.settings.pagination
    document.getElementById('share_link').checked = Ctx.settings.share_link
    document.getElementById('disable_web_view').checked = Ctx.settings.disable_web_view
    document.getElementById('follow').checked = Ctx.settings.follow
    document.getElementById('pin').checked = Ctx.settings.pin
}

function set_page() {
    // Initializing form
    const form = document.getElementById('form')
    form.addEventListener('submit', submit_listener)

    // ... resetters
    const resetters = document.getElementsByClassName('field_resetter')
    Array.from(resetters).forEach(btn => btn.addEventListener('click', (e) => { reset_field(e.target.name); e.preventDefault() }))
    const reset_all = document.getElementById('reset_all')
    reset_all.addEventListener('click', (e) => {
        Object.keys(Defaults).forEach(k => {
            reset_field(k)
        })
        e.preventDefault()
    })

    // ... loader
    const reloader = document.getElementById('reload')
    reloader.addEventListener('click', (e) => {
        asssign_from_Ctx()
        e.preventDefault()
    })

    // ... meta data
    document.getElementById('access_token').innerHTML = "Access token: " + Ctx.access_token
    document.getElementById('chat_id').innerHTML = "Chat Id: " + Ctx.chat_id

    // ... helper
    every_s = document.getElementById('digest_every_secs')
    every_s.addEventListener('keyup', helper_listener)

    asssign_from_Ctx()
}

function receive_payload() {
    const params = new Proxy(new URLSearchParams(window.location.search), {
        get: (searchParams, prop) => searchParams.get(prop),
    })
    const access_token = params.access_token
    Ctx.request_settings(access_token).then(resp => resp.json()).then(payload => {
        if (payload.hasOwnProperty('read_resp_error')) {
            alert("Unable to authenticate your, because of this error", payload.read_resp_error)
            return
        } else {
            Ctx.settings = payload.read_resp_settings
            Ctx.chat_id = payload.read_resp_cid
            Ctx.access_token = access_token
            set_page()
            //start_counter()
        }
    }).catch(e => alert(e))
}

window.onload = receive_payload