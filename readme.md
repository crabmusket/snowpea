# snowpea

Host your own podcast feed

## Story

I'm a big fan of Dan Carlin's [Hardcore History](https://www.dancarlin.com/hardcore-history-series/) and [Common Sense](https://www.dancarlin.com/common-sense-home-landing-page/) podcasts.
I wanted to host my own private feed of the episodes I'd purchased so I could listen to them in [Pocket Casts](https://pocketcasts.com/) instead of copying files onto my phone directly.

This proved to be a bit trickier than I'd imagined, because I wanted to keep my files secure with password protection.
Luckily, Pocket Casts supports basic authentication... but only on the podcast feed itself, not on the individual episode files.

Instead of hosting the podcast feed in one auth-protected directory and the files in another unprotected directory, I decided to do some overengineering, and snowpea was born.

## How it works

Snowpea allows you to host a podcast with basic HTTP authentication on the podcast feed itself, but not on the episode files.
To protect the episodes, snowpea creates unique [JWTs](https://jwt.io) for each episode, and provides links with the JWT attached.

This means that Pocket Casts can download the episode files, because they are not protected by basic auth, but it makes it impossible to steal episode files without having accessed the auth-protected feed.

Each JWT contains the podcast and episode ID, so even if one episode's token is leaked, the token cannot be reused to download other episodes.

## How to host your podcast

1. Get snowpea either by downloading the latest [release](https://github.com/crabmusket/snowpea/releases) or by compiling it yourself following the instructions below
2. Copy snowpea to your server
3. Create a storage directory on your server; see [./podcasts](./podcasts) for instructions
4. Generate a secret for signing your JWTs, e.g. `cat /dev/urandom | tr -dc 'a-zA-Z0-9' | fold -w 32 | head -n 1` (make sure it's at least 32 chars; see [this](https://auth0.com/blog/brute-forcing-hs256-is-possible-the-importance-of-using-strong-keys-to-sign-jwts/))
5. Run snowpea
6. Put the URL in your podcast app
7. Listen to a podcast

## How to contribute

Snowpea is developed using [stack](https://docs.haskellstack.org/en/stable/README/).
Once you have `stack` installed, simply `stack init` and `stack build`.

## Developer guide

Snowpea uses Wai without any higher-level framework on top.
See this excellent [introduction](https://www.yesodweb.com/book/web-application-interface) to learn about the foundational `Application` type.

## Future work

* A web/client interface to easily upload new episodes
* Multiple users/accounts for sharing private podcasts with multiple people
* Improving the Atom/XML feed template to have more variables and be more compliant
