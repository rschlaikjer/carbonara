node {

    def app

    stage('Clone repository') {
        checkout scm
        sh "git rev-parse origin/${sha1} > .commit"
    }

    stage('Inject credentials') {
        sh "cp /creds/carbonara/prod.config files/prod.config"
    }

    stage('Build image') {
        app = docker.build("erlang-carbonara")
    }

    stage('Push image') {
        def commit = readFile('.commit').trim()
        docker.withRegistry('http://docker-registry:5000') {
            app.push("${commit}")
            app.push("${sha1}")
            app.push("latest")
        }

        sh "docker image prune -fa --filter 'until=240h'"
    }

    stage('Kubernetes Deploy') {
        def commit = readFile('.commit').trim()
        sh "docker run --rm \
        --mount type=bind,source=/creds/kubectl/,target=/root/.kube/ \
        lachlanevenson/k8s-kubectl:v1.10.2 \
        set image deployment/carbonara-deployment \
        carbonara=docker-registry:5000/erlang-carbonara:${commit}"

        sh "docker run --rm \
        --mount type=bind,source=/creds/kubectl/,target=/root/.kube/ \
        lachlanevenson/k8s-kubectl:v1.10.2 \
        rollout status deployment/carbonara-deployment"
    }

}

