.PHONY: build
build:
	@docker build -t docker.io/valtteri/onionfermenter .

.PHONY: push
push:
	@docker push docker.io/valtteri/onionfermenter

RANDOM := $(shell bash -c 'echo $$RANDOM')

.PHONY: run
run:
	@docker run \
	--rm \
	--detach \
	-e VICTIM_ONION_ID=${VICTIM_ONION_ID} \
	--name ${VICTIM_ONION_ID}-${RANDOM} \
	--mount type=bind,source="${ADDRESS_FILE}",target=/onionfermenter/BTC-ADDRESSES.txt,readonly \
	docker.io/valtteri/onionfermenter:latest

RELEASE_NAME := $(shell echo ${VICTIM_ONION_ID} |cut -c -53)
NREPLICAS ?= 1

.PHONY: deploy
deploy:
	@helm upgrade \
	--install \
	--set victimOnionId=${VICTIM_ONION_ID} \
	--set fullnameOverride=${VICTIM_ONION_ID} \
	--set replicaCount=${NREPLICAS} \
	--create-namespace \
	--namespace onionfermenter \
	${RELEASE_NAME} \
	./deploy/onionfermenter
	@kubectl create \
	configmap btc-addresses \
	--namespace onionfermenter \
	--from-file=BTC-ADDRESSES.txt=${ADDRESS_FILE} \
	--dry-run=client -o yaml \
	| kubectl apply -f -

.PHONY: get-addresses
get-addresses:
	@kubectl get pods -n onionfermenter -o custom-columns=name:metadata.name --no-headers\
		|grep "${RELEASE_NAME}" \
		|xargs -I{} kubectl -n onionfermenter -c onionfermenter exec {} -- cat /var/lib/tor/hidden_service/hostname

.PHONY: get-addresses-docker
get-addresses-docker:
	@docker ps \
	|grep ${VICTIM_ONION_ID} \
	| awk '{print $$1}' \
	| xargs -I{} docker exec {} cat /var/lib/tor/hidden_service/hostname
