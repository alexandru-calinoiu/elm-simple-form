require 'rack/app'
require 'rack/cors'
require 'json'

use Rack::Cors do
    allow do
        origins '*'
        resource '*', {
            headers: :any,
            methods: [:post]
        }
    end
end

class Server < Rack::App
    post '/api/contact' do
        json = JSON.parse(payload)
        email = json['email']

        response = if email == 'test@test.com'
            { success: true }
        else
            { success: false, errors: ["email_already_registered"] }
        end
        
        JSON.dump(response)
    end
end

run Server