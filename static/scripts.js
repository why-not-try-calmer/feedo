
const Ctx = {
    settings: {},
    chat_id: null,
    acess_token: "",
    base_url: 'https://feedfarer-webapp.azurewebsites.net'
}

const headers = {
    'Accept': 'application/json',
    'Content-Type': 'application/json'
}

const authorize = async (token) => {
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

const send_payload = async (payload) => {
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

const submit_listener = async (e) => {
    const payload = {
        blacklist: form.elements.blacklist.value,
        digest_at: form.elements.digest_at.value,
        digest_collapse: form.elements.digest_collapse.value,
        digest_every: form.elements.digest_every.value,
        digest_size: form.elements.digest_size.value,
        digest_start: form.elements.digest_start.value,
        digest_title: form.elements.digest_title.value,
        disable_webview: form.elements.disable_webview.value,
        follow: form.element.follow.value,
        only_search_notif: form.element.search_notif.value,
        pin: form.element.pin.value,
        search_notif: form.element.search_notif.value,
        share_link: form.element.share_link.value
    }
    await send_payload(payload)
    e.preventDefault()
}

const start_counter = (counter = 300) => {
    const handler = () => {
        counter--
        if (counter <= 0) document.getElementById('time_left').innerHTML = 'Time elapsed!'
        else document.getElementById('time_left').innerHTML = counter.toString()
    }
    setTimeout(() => counter * 1000)
    setInterval(handler, 1000)
}

const set_page = () => {
    const form = document.querySelector('form')
    form.addEventListener('submit', submit_listener)

    document.getElementById('access_token').innerHTML = "Access token: " + Ctx.access_token
    document.getElementById('chat_id').innerHTML = "Chat Id: " + Ctx.chat_id
    document.getElementById('showcase').innerHTML = JSON.stringify(Ctx.settings)
    
    document.getElementById('blacklist').value = Ctx.settings.blacklist
    
    const digest_at = Ctx.settings.digest_at.map(v => {
        let [h, m] = v
        if (h < 10) h = "0" + h
        if (m < 10) m = "0" + m
        return `${h.toString()}:${m.toString()}`
    })
    document.getElementById('digest_at').value = digest_at
    document.getElementById('digest_size').value = Ctx.settings.digest_size
    document.getElementById('digest_title').value = Ctx.settings.digest_title
    document.getElementById('digest_collapse').value = Ctx.settings.digest_collapse
    
    document.getElementById('search_notif').value = Ctx.settings.search_notif
    document.getElementById('only_search_notif').value = Ctx.settings.only_search_notif
    
    document.getElementById('share_link').checked = Ctx.settings.share_link
    document.getElementById('disable_webview').checked = Ctx.settings.disable_webview
    document.getElementById('follow').checked = Ctx.settings.follow
    document.getElementById('pin').checked = Ctx.settings.pin

    const seconds_to_int = () => {
        if (Ctx.settings.digest_every % 86400 == 0) return { x: Ctx.settings / 86400, y: 'd'}
        if (Ctx.settings.digest_every % 3600 == 0) return { x: Ctx.settings / 3600, y:'h'}
        if (Ctx.settings.digest_every % 60 == 0) return { x: Ctx.settings / 60, y:'m' }
        return { x: 0, y: 'd'}
    }
    const {x, y} = seconds_to_int()
    document.getElementById('every_n').value = x
    document.getElementById('select_every').value = y
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